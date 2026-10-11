# =============================================================================
# fn_boot.R  bootstrap (並列計算・キャッシュ・optimism 補正・location-shifted CI)
# -----------------------------------------------------------------------------
# bootstrap による内部検証 (Harrell の optimism 補正, §7.3・§7.5) の考え方:
#   1. 元のデータから同じ人数を「復元抽出」して bootstrap 標本を作る
#   2. その標本で λ の選択からモデルの推定までをやり直す
#   3. 推定したモデルの性能を、(a) その標本自身 と (b) 元のデータ で測る
#   4. (a) − (b) が optimism (自分の学習データでは良く見えてしまう分)。これを B 回平均する
#   5. 元のデータでの見かけの性能 − 平均の optimism = optimism 補正した性能
#
# 速くするための工夫:
#   - 回ごとの計算を複数の CPU コアに分担させる (parallel パッケージ。Windows でも動く)
#   - bootstrap 標本の行番号と CV の fold は、並列化の前にまとめて乱数で作っておく。
#     各コアは乱数を使わないので、コア数を変えても結果はまったく同じになる (再現性)
#   - 計算した結果はファイルに保存し、設定とデータが同じなら次回は読み込むだけにする
# =============================================================================

# ---- bootstrap 標本と CV の fold をまとめて作る ----
# n    : 元のデータの人数
# B    : bootstrap の回数
# seed : 乱数種 (アウトカムごとに変えて、互いに独立にする)
# 戻り値: list(idx = 行番号の行列 [n × B], folds = 元の個人ごとの fold [n × 繰り返し × B])
make_boot_design <- function(n, B, seed, n_repeat = N_CV_REPEAT, k = N_FOLD) {
  set.seed(seed)
  idx <- matrix(sample.int(n, n * B, replace = TRUE), nrow = n, ncol = B)
  folds <- array(NA_integer_, dim = c(n, n_repeat, B))
  for (b in seq_len(B)) folds[, , b] <- make_folds(n, n_repeat, k)
  list(idx = idx, folds = folds)
}

# ---- キャッシュ ----
# 同じ名前・同じ key (設定とデータから作った指紋) の結果がすでに保存されていれば読み込み、
# なければ expr を計算して保存する。REBUILD_BOOT <- TRUE (00_setup.R) で必ず計算し直す。
# 保存するのは集計済みの指標だけで、個人単位のデータは含まない。
run_cached <- function(name, key, expr) {
  path <- file.path(DIR_DERIVED, "cache", paste0(name, ".rds"))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (!REBUILD_BOOT && file.exists(path)) {
    old <- readRDS(path)
    if (identical(attr(old, "cache_key"), key)) {
      message("  キャッシュを読み込みました: ", name)
      return(old)
    }
  }
  t0 <- Sys.time()
  out <- expr                      # ここで初めて計算が走る (R の遅延評価)
  attr(out, "cache_key") <- key
  saveRDS(out, path)
  message("  計算して保存しました: ", name, " (", round(difftime(Sys.time(), t0, units = "mins"), 1), " 分)")
  out
}

# 共通関数 (code/analysis/R/*.R) のコードの指紋。キャッシュの key に含める
code_fingerprint <- function() {
  rlang::hash(lapply(sort(list.files("code/analysis/R", pattern = "\\.R$", full.names = TRUE)),
                     readLines, warn = FALSE))
}

# ---- 並列計算 ----
# X の要素ごとに FUN(要素, spec) を実行する (parLapply の負荷分散版)。
# 各コア (ワーカー) で glmnet と共通関数を読み込み、spec (データ・bootstrap 標本など) は
# 最初に 1 回だけ各ワーカーに送っておく (回ごとに送ると、本番では数 GB の受け渡しになり遅くなる)。
par_map <- function(X, FUN, spec, n_cores = N_CORES) {
  n_cores <- min(n_cores, length(X))
  if (n_cores <= 1) return(lapply(X, FUN, spec = spec))
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)
  # Windows では、画面を持たないワーカーのプロセスが「バックグラウンド」とみなされ、
  # 省電力のコア・低いクロックで動かされて数倍遅くなることがある (Intel の混成コアの PC で確認)。
  # ワーカーの優先度を「通常より上」にしてこれを避ける。失敗しても計算は続ける
  if (.Platform$OS.type == "windows") {
    pids <- unlist(clusterCall(cl, Sys.getpid))
    try(system2("powershell", c("-NoProfile", "-Command", shQuote(sprintf(
      "Get-Process -Id %s | ForEach-Object { $_.PriorityClass = 'AboveNormal' }",
      paste(pids, collapse = ",")))), stdout = FALSE, stderr = FALSE), silent = TRUE)
  }
  fn_files <- normalizePath(list.files("code/analysis/R", pattern = "\\.R$", full.names = TRUE))
  clusterCall(cl, function(files, s) {
    suppressPackageStartupMessages(library(glmnet))
    for (f in files) source(f, encoding = "UTF-8")
    assign(".spec", s, envir = globalenv())      # spec を各ワーカーに 1 回だけ置く
    NULL
  }, fn_files, spec)
  # ワーカーで実行する関数。環境をグローバルにしておく (par_map の中の変数 [spec など] を
  # 関数と一緒に毎回送ってしまわないように)。引数名は parLapplyLB の引数 (fun) と重ならない名前にする
  run_one <- function(i, worker_fun) worker_fun(i, get(".spec", envir = globalenv()))
  environment(run_one) <- globalenv()
  parLapplyLB(cl, X, run_one, worker_fun = FUN)
}

# ---- 開発データの bootstrap 1 回分 ----
# spec の中身:
#   y      : アウトカム (0/1, 元のデータ)
#   X      : モデルごとのデザイン行列のリスト
#   grid   : モデルごとの λ の候補列のリスト
#   idx, folds : make_boot_design() の結果
#   curve_models: 較正曲線を計算するモデルの名前
#   components  : 構成要素ごとの対象とアウトカム (make_components() の結果。E9)。NULL なら計算しない
#   comp_models : 構成要素ごとの AUC を計算するモデルの名前
# 戻り値: list(metrics = モデルごとの指標 1 行, curves = 較正曲線の格子点の値,
#              comp = 構成要素ごとの AUC [その標本自身・元のデータ])
boot_dev_one <- function(b, spec) {
  idx  <- spec$idx[, b]
  # 元の個人の fold を、複製にもそのまま引き継ぐ (行 = 標本の行、列 = CV の繰り返し)
  fold <- matrix(spec$folds[idx, , b], nrow = length(idx))
  yb   <- spec$y[idx]
  metrics <- list(); curves <- list(); comp <- list()
  for (m in names(spec$X)) {
    X  <- spec$X[[m]]
    Xb <- X[idx, , drop = FALSE]
    # 選んだ λ で推定できなかった回は失敗として記録し (指標は NA)、集計から除く (07 で件数を確認する)
    fit <- tryCatch(fit_ridge_cv(Xb, yb, fold, spec$grid[[m]]), error = function(e) NULL)
    if (is.null(fit)) {
      metrics[[m]] <- data.frame(b = b, model = m, lambda = NA_real_, lambda_index = NA_integer_,
                                 cv_truncated = NA_integer_, fit_truncated = NA,
                                 auc_app = NA_real_, auc_test = NA_real_, slope_app = NA_real_,
                                 slope_test = NA_real_, citl_app = NA_real_, citl_test = NA_real_)
      next
    }
    lp_boot <- predict_lp(fit$coef, Xb)   # (a) 自分の標本での性能
    lp_orig <- predict_lp(fit$coef, X)    # (b) 元のデータでの性能
    app  <- perf_stats(yb, lp_boot)
    test <- perf_stats(spec$y, lp_orig)
    metrics[[m]] <- data.frame(b = b, model = m, lambda = fit$lambda, lambda_index = fit$lambda_index,
                               cv_truncated = fit$cv_truncated, fit_truncated = fit$fit_truncated,
                               auc_app = app[["auc"]], auc_test = test[["auc"]],
                               slope_app = app[["slope"]], slope_test = test[["slope"]],
                               citl_app = app[["citl"]], citl_test = test[["citl"]])
    if (m %in% spec$curve_models) {
      curves[[m]] <- data.frame(b = b, model = m, p = CALIB_GRID,
                                app = calib_curve(yb, plogis(lp_boot)),
                                test = calib_curve(spec$y, plogis(lp_orig)))
    }
    # 構成要素ごとの AUC (E9)。複合アウトカムで推定したモデルを、構成要素の対象で評価する。
    # (a) その標本の中の構成要素の対象、(b) 元のデータの構成要素の対象
    if (m %in% spec$comp_models) {
      for (cn in names(spec$components)) {
        cc  <- spec$components[[cn]]
        inb <- cc$in_pop[idx]
        comp[[length(comp) + 1]] <- data.frame(
          b = b, model = m, component = cn,
          auc_app  = auc_fast(cc$y[idx][inb], lp_boot[inb]),
          auc_test = auc_fast(cc$y[cc$in_pop], lp_orig[cc$in_pop]))
      }
    }
  }
  list(metrics = do.call(rbind, metrics), curves = do.call(rbind, curves), comp = do.call(rbind, comp))
}

# ---- location-shifted bootstrap CI (§7.5) ----
# bootstrap 標本での見かけの性能 (app) の分布の 2.5・97.5 パーセンタイルを、
# optimism の分だけずらした区間 (Noma ら 2021 [PMID 34302372])
ls_ci <- function(app_boot, optimism, level = 0.95) {
  a <- (1 - level) / 2
  unname(quantile(app_boot, c(a, 1 - a), na.rm = TRUE) - optimism)
}

# ---- optimism 補正の集計 ----
# orig  : 元のデータでの見かけの性能 (最終モデル)。model, auc, slope, citl の data frame
# boot  : boot_dev_one() の metrics をまとめたもの
# 戻り値: モデル × 指標ごとに、見かけの値・optimism・補正値・95% CI
# 95% CI (location-shifted) は AUC だけに付ける (§7.5)。較正の slope・CITL には付けない
#   (リッジ回帰は切片に罰則をかけないので、自分の標本での CITL はつねにほぼ 0 になり、
#    この方法の区間は意味をもたない)。較正は bootstrap での分布を別に示す (boot_dist)
summarise_optimism <- function(orig, boot) {
  out <- list()
  for (st in c("auc", "slope", "citl")) {
    bs <- boot |>
      group_by(model) |>
      summarise(optimism = mean(.data[[paste0(st, "_app")]] - .data[[paste0(st, "_test")]], na.rm = TRUE),
                app_boot = list(.data[[paste0(st, "_app")]]), .groups = "drop")
    out[[st]] <- orig |>
      select(model, apparent = all_of(st)) |>
      inner_join(bs, by = "model") |>
      mutate(stat = st, corrected = apparent - optimism,
             lo = if (st == "auc") map2_dbl(app_boot, optimism, \(a, o) ls_ci(a, o)[1]) else NA_real_,
             hi = if (st == "auc") map2_dbl(app_boot, optimism, \(a, o) ls_ci(a, o)[2]) else NA_real_) |>
      select(-app_boot)
  }
  bind_rows(out)
}

# ΔAUC (モデル 1 − モデル 2) の optimism 補正。同じ bootstrap 標本の中で対にして差をとる (§7.5)
summarise_delta <- function(orig, boot, pairs) {
  map_dfr(seq_len(nrow(pairs)), \(i) {
    m1 <- pairs$model1[i]; m2 <- pairs$model2[i]
    w <- boot |>
      filter(model %in% c(m1, m2)) |>
      select(b, model, auc_app, auc_test) |>
      pivot_wider(names_from = model, values_from = c(auc_app, auc_test))
    d_app  <- w[[paste0("auc_app_", m1)]]  - w[[paste0("auc_app_", m2)]]
    d_test <- w[[paste0("auc_test_", m1)]] - w[[paste0("auc_test_", m2)]]
    opt <- mean(d_app - d_test, na.rm = TRUE)
    apparent <- orig$auc[orig$model == m1] - orig$auc[orig$model == m2]
    ci <- ls_ci(d_app, opt)
    tibble(contrast = pairs$contrast[i], model1 = m1, model2 = m2,
           apparent = apparent, optimism = opt, corrected = apparent - opt, lo = ci[1], hi = ci[2],
           n_pairs = sum(!is.na(d_app) & !is.na(d_test)))   # 両モデルとも成功した bootstrap の回数
  })
}

# ---- 開発データでの推定と bootstrap 内部検証をまとめて行う ----
# 03 (主解析) と 05 (感度分析 S2・S3) で同じ手順を使うための関数。
# d           : 解析対象 (結果が確定した人だけ、1 人 1 行)
# y           : アウトカム (0/1)
# models      : 推定するモデルの名前 (MODELS の名前)
# seed_final  : 最終モデルの CV 分割の乱数種
# seed_boot   : bootstrap 標本と各標本内の CV 分割の乱数種
# cache_name  : キャッシュのファイル名
# curve_models: 較正曲線を計算するモデル
# components  : 構成要素ごとの対象とアウトカム (make_components() の結果。E9)。NULL なら計算しない
# comp_models : 構成要素ごとの AUC を計算するモデル
# 戻り値: list(final = モデルごとの最終モデル, orig = 見かけの性能, metrics = bootstrap 各回の指標,
#              curves = bootstrap 各回の較正曲線, comp = bootstrap 各回の構成要素ごとの AUC,
#              grid = λ の候補列)
run_dev_bootstrap <- function(d, y, models, seed_final, seed_boot, cache_name,
                              curve_models = character(0), components = NULL,
                              comp_models = character(0)) {
  X    <- map(MODELS[models], \(v) make_x(d, v))   # モデルごとのデザイン行列
  grid <- map(X, \(x) make_lambda_grid(x, y))      # モデルごとの λ の候補列

  # 最終モデル: 開発データ全体で λ を選んで推定する。CV 分割は全モデルで共通 (§7.5)
  set.seed(seed_final)
  folds_full <- make_folds(nrow(d))
  final <- imap(X, \(x, m) {
    fit <- fit_ridge_cv(x, y, folds_full, grid[[m]])
    lp  <- predict_lp(fit$coef, x)
    list(model = m, vars = MODELS[[m]], coef = fit$coef, lambda = fit$lambda,
         lambda_index = fit$lambda_index, lambda_grid_range = range(grid[[m]]),
         cv_truncated = fit$cv_truncated, fit_truncated = fit$fit_truncated,
         coef_std = fit$coef[-1] * apply(x, 2, sd),   # 標準化係数 (開発データの SD 1 単位あたり)
         x_colnames = colnames(x), apparent = perf_stats(y, lp), lp = lp)
  })
  orig <- map_dfr(final, \(f) tibble(model = f$model, auc = f$apparent[["auc"]],
                                      slope = f$apparent[["slope"]], citl = f$apparent[["citl"]]))

  # bootstrap: 標本と fold は先にまとめて作り、並列に計算する。結果はキャッシュする
  design <- make_boot_design(nrow(d), N_BOOT, seed = seed_boot)
  if (is.null(components)) comp_models <- character(0)
  spec <- list(y = y, X = X, grid = grid, idx = design$idx, folds = design$folds,
               curve_models = curve_models, components = components, comp_models = comp_models)
  # key には、データ・設定に加えて共通関数のコードも含める (関数を直したら計算し直すように)
  key <- rlang::hash(list(y, X, grid, design, N_CV_REPEAT, N_FOLD, CALIB_GRID, curve_models,
                          components, comp_models, seed_final, code_fingerprint()))
  boot <- run_cached(cache_name, key, par_map(seq_len(N_BOOT), boot_dev_one, spec))
  list(final = final, orig = orig,
       metrics = bind_rows(map(boot, "metrics")), curves = bind_rows(map(boot, "curves")),
       comp = bind_rows(map(boot, "comp")), grid = grid)
}

# ---- 構成要素ごとの判別能の optimism 補正 (E9, §7.5) ----
# final      : run_dev_bootstrap() の final (個人ごとの lp を含む)
# components : make_components() の結果
# comp_boot  : run_dev_bootstrap() の comp
# pairs      : ΔAUC を求めるモデルの組 (contrast, model1, model2)
# 戻り値: list(perf = 構成要素 × モデルの AUC [見かけ・optimism・補正値・95% CI・人数・イベント数],
#              delta = 構成要素 × 組の ΔAUC)
# AUC の CI と ΔAUC の補正は、複合アウトカムと同じ方法 (location-shifted CI、同じ標本の中で対にする)
summarise_components <- function(final, components, comp_boot, pairs) {
  models <- unique(comp_boot$model)
  perf <- list(); delta <- list()
  for (cn in names(components)) {
    cc <- components[[cn]]
    orig <- map_dfr(models, \(m) tibble(model = m, auc = auc_fast(cc$y[cc$in_pop], final[[m]]$lp[cc$in_pop])))
    bs <- filter(comp_boot, component == cn)
    perf[[cn]] <- bs |>
      group_by(model) |>
      summarise(optimism = mean(auc_app - auc_test, na.rm = TRUE), app_boot = list(auc_app), .groups = "drop") |>
      inner_join(orig, by = "model") |>
      mutate(component = cn, apparent = auc, corrected = apparent - optimism,
             lo = map2_dbl(app_boot, optimism, \(a, o) ls_ci(a, o)[1]),
             hi = map2_dbl(app_boot, optimism, \(a, o) ls_ci(a, o)[2]),
             n = sum(cc$in_pop), events = sum(cc$y[cc$in_pop])) |>
      select(component, model, apparent, optimism, corrected, lo, hi, n, events)
    delta[[cn]] <- summarise_delta(orig, bs, filter(pairs, model1 %in% models, model2 %in% models)) |>
      mutate(component = cn)
  }
  list(perf = bind_rows(perf), delta = bind_rows(delta))
}

# ---- bootstrap の健全性の集計 (内部診断, §7.13 (c) 4) ----
# metrics: boot_dev_one() の metrics をまとめたもの、grid: モデルごとの λ の候補列
# 戻り値: モデルごとに、試行数・成功数・失敗数・λ の列の途中停止・λ が候補列の端に選ばれた数
summarise_boot_health <- function(metrics, grid) {
  metrics |>
    mutate(n_grid = lengths(grid)[model]) |>
    group_by(model) |>
    summarise(n_boot = n(),
              n_success = sum(!is.na(auc_test) & !is.na(slope_test)),
              n_failed = n_boot - n_success,
              n_cv_truncated = sum(cv_truncated > 0, na.rm = TRUE),
              n_fit_truncated = sum(fit_truncated, na.rm = TRUE),
              n_lambda_at_max = sum(lambda_index == 1, na.rm = TRUE),
              n_lambda_at_min = sum(lambda_index == n_grid, na.rm = TRUE),
              .groups = "drop")
}
