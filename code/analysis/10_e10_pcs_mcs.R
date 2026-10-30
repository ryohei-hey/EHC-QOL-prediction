# =============================================================================
# 10_e10_pcs_mcs.R  健診前 PCS と MCS の上乗せの分解 (E10。開発の bootstrap と外部検証)
# -----------------------------------------------------------------------------
# 目的 : 統合モデル (健診 + 健診前 PCS・MCS, E7) の上乗せが、PCS と MCS のどちらによるかを見る。
#        健診ベースモデルに健診前の PCS だけ・MCS だけを加えたモデルを、約 1 年半後の
#        PCS 複合・MCS 複合について 03 と同じ手順 (リッジ回帰、λ の選択を含む bootstrap) で推定し、
#        ΔAUC と構成要素ごとの判別能 (E9 と同じ方法) を、開発 (optimism 補正) と外部検証 (V) で求める。
# 入力 : data/processed/analysis_sets.rds (01)、output/derived/cache/boot_dev_*.rds (03。一致の確認だけに使う)
# 出力 : output/derived/e10_pcs_mcs.rds  開発: optimism 補正 AUC・ΔAUC・構成要素ごとの AUC と ΔAUC、
#                                        外部検証: AUC・slope・CITL・ΔAUC (V・年度別)、構成要素ごとの AUC と ΔAUC (V)
#        output/derived/cache/boot_e10_*.rds bootstrap 各回の指標 (キャッシュ)
# 対応 : protocol §3.4 E10・§7.5 E10
# 方法 : 乱数種は 03 と同じ (アウトカムの番号 k から SEED + k・SEED + 100 × k) にするため、bootstrap 標本と
#        CV の分割は 03 とまったく同じになる。健診ベースモデルと統合モデルも同じ標本で推定し直して対にする。
#        その 2 モデルの結果が 03 のキャッシュと一致することを確かめる (§7 の手順が同じであることの確認)
# 時間 : 本番 (N_BOOT = 500) で 20 コアなら 1 アウトカム約 20 分、計 40 分前後
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
as_sets <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))
D <- as_sets$D; V <- as_sets$V
MODELS <- c(MODELS, MODELS_E10)   # このファイルの中だけで E10 のモデルを加える
message("N_BOOT = ", N_BOOT, "、コア数 = ", N_CORES)

E10_MODELS <- c("checkup", "checkup_pcs", "checkup_mcs", "checkup_direct")
# 比べるモデルの組 (ΔAUC = model1 − model2)。E7 は 03・04 と同じ値になる (確認用)
E10_PAIRS <- tribble(
  ~contrast,           ~model1,          ~model2,
  "E10_pcs",           "checkup_pcs",    "checkup",       # 健診 + PCS − 健診
  "E10_mcs",           "checkup_mcs",    "checkup",       # 健診 + MCS − 健診
  "E10_mcs_given_pcs", "checkup_direct", "checkup_pcs",   # 統合 − 健診 + PCS (PCS に加えた MCS の上乗せ)
  "E10_pcs_given_mcs", "checkup_direct", "checkup_mcs",   # 統合 − 健診 + MCS (MCS に加えた PCS の上乗せ)
  "E7",                "checkup_direct", "checkup"        # 統合 − 健診 (確認用)
)

# ---- 1. 開発: 最終モデルと bootstrap 内部検証 (約 1 年半後だけ) ----
dev <- list(); final_all <- list()
for (k in which(OUTCOMES$time == "18m")) {
  oc <- OUTCOMES[k, ]
  message("\n==== ", oc$y, " ====")
  d <- D |> filter(!is.na(.data[[oc$y]]))
  y <- d[[oc$y]]
  comps <- make_components(d, oc$domain, oc$time)
  # 乱数種は 03 と同じ (k は OUTCOMES の行番号で、03 のループの番号と同じ)
  res <- run_dev_bootstrap(d, y, E10_MODELS, seed_final = SEED + 100 * k, seed_boot = SEED + k,
                           cache_name = paste0("boot_e10_", oc$y),
                           components = comps, comp_models = E10_MODELS)

  # 03 のキャッシュとの一致の確認 (健診ベースモデル・統合モデルの bootstrap 各回の AUC)
  old_path <- file.path(DIR_DERIVED, "cache", paste0("boot_dev_", oc$y, ".rds"))
  if (file.exists(old_path)) {
    old <- bind_rows(map(readRDS(old_path), "metrics"))
    chk <- res$metrics |>
      filter(model %in% c("checkup", "checkup_direct")) |>
      inner_join(old, by = c("b", "model"), suffix = c("", "_03"))
    max_diff <- max(abs(c(chk$auc_app - chk$auc_app_03, chk$auc_test - chk$auc_test_03)), na.rm = TRUE)
    message("  03 のキャッシュとの差 (AUC の最大の絶対差、", nrow(chk), " 行): ", signif(max_diff, 3))
    if (nrow(chk) == 0 || max_diff > 1e-8) warning("03 の bootstrap と一致しません: ", oc$y)
  }
  print(summarise_boot_health(res$metrics, res$grid))

  perf  <- summarise_optimism(res$orig, res$metrics) |> mutate(outcome = oc$y, n = length(y), events = sum(y))
  delta <- summarise_delta(res$orig, res$metrics, E10_PAIRS) |> mutate(outcome = oc$y)
  comp  <- summarise_components(res$final, comps, res$comp, E10_PAIRS)
  dev[[oc$y]] <- list(perf = perf, delta = delta,
                      comp = mutate(comp$perf, outcome = oc$y), comp_delta = mutate(comp$delta, outcome = oc$y))
  # 最終モデルは、個人ごとの予測値 (lp) を除いて保存する
  final_all[[oc$y]] <- map(res$final, \(f) f[names(f) != "lp"])
  print(delta |> select(contrast, apparent, optimism, corrected, lo, hi))
}

# ---- 2. 外部検証 (V、年度別も) ----
# 最終モデルの係数を固定して当てはめる (04 と同じ)
lp_of <- function(d, yv, m) {
  f <- final_all[[yv]][[m]]
  x <- make_x(d, f$vars)
  stopifnot(identical(colnames(x), f$x_colnames))
  predict_lp(f$coef, x)
}
eval_sets <- list(V = V, V_FY2022 = filter(V, val_fy == "FY2022"), V_FY2023 = filter(V, val_fy == "FY2023"))
perf_ext <- list(); delta_ext <- list(); comp_ext <- list(); comp_delta_ext <- list()
for (s in names(eval_sets)) {
  for (dom in c("pcs", "mcs")) {
    yv <- paste0("y_", dom, "_18m")
    d  <- eval_sets[[s]] |> filter(!is.na(.data[[yv]]))
    y  <- d[[yv]]
    lps <- map(set_names(E10_MODELS), \(m) lp_of(d, yv, m))
    perf_ext[[length(perf_ext) + 1]] <- imap_dfr(lps, \(lp, m) ext_perf(y, lp) |> mutate(model = m)) |>
      mutate(set = s, outcome = yv)
    delta_ext[[length(delta_ext) + 1]] <- pmap_dfr(E10_PAIRS, \(contrast, model1, model2)
      ext_delta(y, lps[[model1]], lps[[model2]]) |>
        mutate(contrast = contrast, model1 = model1, model2 = model2)) |>
      mutate(set = s, outcome = yv)

    # 構成要素ごとの判別能 (E9 と同じ対象。V [年度を統合] だけ)
    if (s == "V") {
      comps <- make_components(d, dom, "18m")
      for (cn in names(comps)) {
        keep <- comps[[cn]]$in_pop
        yc   <- comps[[cn]]$y[keep]
        comp_ext[[length(comp_ext) + 1]] <- imap_dfr(lps, \(lp, m)
          ext_perf(yc, lp[keep]) |> filter(stat == "auc") |> mutate(model = m)) |>
          mutate(component = cn, set = s, outcome = yv)
        comp_delta_ext[[length(comp_delta_ext) + 1]] <- pmap_dfr(E10_PAIRS, \(contrast, model1, model2)
          ext_delta(yc, lps[[model1]][keep], lps[[model2]][keep]) |>
            mutate(contrast = contrast, model1 = model1, model2 = model2)) |>
          mutate(component = cn, set = s, outcome = yv)
      }
    }
  }
}
perf_ext <- bind_rows(perf_ext); delta_ext <- bind_rows(delta_ext)
comp_ext <- bind_rows(comp_ext); comp_delta_ext <- bind_rows(comp_delta_ext)
print(delta_ext |> filter(set == "V") |> select(outcome, contrast, estimate, lo, hi))

# ---- 3. 保存 ----
save_derived(list(final = final_all,
                  perf_dev = bind_rows(map(dev, "perf")), delta_dev = bind_rows(map(dev, "delta")),
                  comp_dev = bind_rows(map(dev, "comp")), comp_delta_dev = bind_rows(map(dev, "comp_delta")),
                  perf_ext = perf_ext, delta_ext = delta_ext,
                  comp_ext = comp_ext, comp_delta_ext = comp_delta_ext),
             "e10_pcs_mcs")
message("10 完了")
