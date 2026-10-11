# =============================================================================
# fn_model.R  リッジ回帰の推定 (デザイン行列、λ の選択、推定、予測)
# -----------------------------------------------------------------------------
# プロトコル §7.5 に従う。
#   - リッジ回帰 = L2 罰則つきロジスティック回帰 (glmnet, alpha = 0)
#   - 予測因子は各学習標本の中で標準化してから罰則をかける (glmnet の既定 standardize = TRUE)。
#     係数は glmnet が元の単位に戻して返す
#   - λ の選択: 10-fold CV の逸脱度 (deviance) が最小となる値 (lambda.min)。
#     分割を変えた 10-fold CV を 5 回繰り返し、共通の λ 候補列で平均の逸脱度を比べる
# 速くするための工夫:
#   - formula を毎回解釈しないよう、デザイン行列 (model.matrix) は最初に 1 回だけ作る
#   - cv.glmnet は使わず、fold ごとに glmnet を 1 回当てて逸脱度を直接計算する
#     (cv.glmnet は CV のたびに全データでの推定も行うため、その分の時間を省ける)
# =============================================================================

# ---- デザイン行列 ----
# カテゴリ変数をダミー変数 (0/1) に展開した数値の行列を作る。切片の列は除く。
# factor の水準は 00 で固定してあるので、開発と検証で同じ列が同じ順に並ぶ。
# d    : データ (1 人 1 行)
# vars : 予測因子の名前 (MODELS の要素)
make_x <- function(d, vars) {
  x <- model.matrix(reformulate(vars), data = d)[, -1, drop = FALSE]
  stopifnot(nrow(x) == nrow(d))   # 欠測があると行が落ちるので、行数が変わらないことを確かめる
  x
}

# ---- λ の候補列 ----
# 開発データ全体に glmnet を当てたときの既定の λ の最大値から、既定の最小値のさらに 1/100 まで、
# 対数で等間隔に 120 個 (1 桁あたり 20 個) 並べた列を、その後のすべての推定 (bootstrap 標本を含む) で
# 共通の候補として使う。
# 既定の列 (最大値の 1/10,000 まで) より小さい側に広げるのは、予測因子の少ないモデル (HRQOL モデル:
# 4 パラメータ) では罰則がほとんど要らず、既定の最小値に張りつくことがあるため。
# 動作確認 (bootstrap 20 回) では、HRQOL モデルで選ばれた λ は、既定の最小値より 0.4〜1.4 桁小さく、
# この列の最小の端より 0.6 桁以上大きかった。端に張りついた標本がないかは 07 で確認する (あれば警告)
N_LAMBDA <- 120
make_lambda_grid <- function(x, y) {
  g <- glmnet(x, y, family = "binomial", alpha = 0)$lambda
  exp(seq(log(max(g)), log(min(g) / 100), length.out = N_LAMBDA))
}

# ---- CV の fold の割り当て ----
# 個人 (元の行番号) ごとに fold 番号 1〜10 を割り当てる。n_repeat 回分を列にした行列を返す。
# bootstrap 標本では同じ人が複数回選ばれるので、fold は「元の個人」に割り当て、
# 同じ人の複製が学習側と評価側にまたがらないようにする (§7.5)
make_folds <- function(n, n_repeat = N_CV_REPEAT, k = N_FOLD) {
  sapply(seq_len(n_repeat), \(r) sample(rep_len(seq_len(k), n)))
}

# ---- 逸脱度 (二値アウトカム) ----
# y: 0/1、eta: 線形予測子 (logit)。行列 eta (人 × λ) にも対応し、λ ごとの合計を返す
deviance_binomial <- function(y, eta) {
  p <- plogis(eta)
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)   # log(0) を避ける
  -2 * colSums(y * log(p) + (1 - y) * log(1 - p))
}

# ---- λ の選択 (繰り返し 10-fold CV) ----
# x, y   : 推定に使うデータ (bootstrap 標本ならその標本)
# foldid : 行ごとの fold 番号の行列 (行 = x の行、列 = 繰り返し)
# grid   : λ の候補列
# 戻り値 : list(lambda = 選ばれた λ, index = 候補列の何番目か, cv = 候補ごとの平均逸脱度,
#              n_truncated = λ の列の途中で推定が止まった fold の数)
# glmnet は、λ を小さくしても当てはまりがほとんど変わらなくなると、列の途中で推定をやめることがある。
# その場合、推定されなかった λ は、その CV では選べないもの (逸脱度 = Inf) として扱い、件数を記録する
# (内部診断 07 で確認する)
select_lambda <- function(x, y, foldid, grid) {
  dev_sum <- numeric(length(grid))
  n_trunc <- 0L
  for (r in seq_len(ncol(foldid))) {
    for (k in sort(unique(foldid[, r]))) {
      test <- foldid[, r] == k
      fit <- glmnet(x[!test, , drop = FALSE], y[!test], family = "binomial",
                    alpha = 0, lambda = grid)
      eta <- predict(fit, x[test, , drop = FALSE], type = "link")
      dev <- rep(Inf, length(grid))
      dev[seq_len(ncol(eta))] <- deviance_binomial(y[test], eta)
      if (ncol(eta) < length(grid)) n_trunc <- n_trunc + 1L
      dev_sum <- dev_sum + dev
    }
  }
  cv <- dev_sum / (ncol(foldid) * length(y))   # 1 人あたりの平均逸脱度 (繰り返しで平均)
  i <- which.min(cv)
  list(lambda = grid[i], index = i, cv = cv, n_truncated = n_trunc)
}

# ---- リッジ回帰の推定 ----
# 候補列の最大の λ から選んだ λ までを順に推定し (glmnet は列に沿って順に解くと速く安定する)、
# 選んだ λ の係数を取り出す。
# 戻り値: 切片を先頭にした係数のベクトル (元の単位)。
#   選んだ λ まで推定が届かなかった場合は、選んだ λ ひとつだけで推定し直す (属性 truncated = TRUE)。
#   それでも選んだ λ で推定できなければエラーにする (別の λ の係数で続けない)。
#   bootstrap ではそのエラーを「その回の失敗」として扱い (boot_dev_one)、最終モデルでは解析を止める
fit_ridge <- function(x, y, lambda, grid) {
  path <- grid[grid >= lambda]
  fit <- glmnet(x, y, family = "binomial", alpha = 0, lambda = path)
  truncated <- length(fit$lambda) < length(path)
  if (truncated) {
    fit <- suppressWarnings(glmnet(x, y, family = "binomial", alpha = 0, lambda = lambda))
    if (length(fit$lambda) != 1 || !isTRUE(all.equal(fit$lambda, lambda)))
      stop("選んだ λ でリッジ回帰を推定できませんでした (λ = ", signif(lambda, 4), ")")
  }
  b <- as.numeric(coef(fit, s = lambda, exact = FALSE))
  names(b) <- c("(Intercept)", colnames(x))
  attr(b, "truncated") <- truncated
  b
}

# 線形予測子 (logit) を計算する: 切片 + x %*% 係数
predict_lp <- function(b, x) as.numeric(b[1] + x %*% b[-1])

# ---- λ の選択から推定までを 1 回で行う ----
# 開発データ全体での最終モデルと、bootstrap 標本での推定の両方で使う
fit_ridge_cv <- function(x, y, foldid, grid) {
  sel <- select_lambda(x, y, foldid, grid)
  b <- fit_ridge(x, y, sel$lambda, grid)
  list(coef = c(b), lambda = sel$lambda, lambda_index = sel$index,
       cv_truncated = sel$n_truncated, fit_truncated = attr(b, "truncated"))
}
