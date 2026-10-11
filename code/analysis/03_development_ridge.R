# =============================================================================
# 03_development_ridge.R  モデルの開発と bootstrap による内部検証 (重い計算)
# -----------------------------------------------------------------------------
# 目的 : 4 つのモデル (健診・質問票・健診前 HRQOL・健診 + 健診前 HRQOL) を、
#        4 つのアウトカム (PCS / MCS × 約 1 年半後 / 約半年後) ごとにリッジ回帰で推定し、
#        bootstrap (N_BOOT 回、λ の選択を含む) で optimism 補正した性能を求める。
# 入力 : data/processed/analysis_sets.rds (01)
# 出力 : output/derived/final_models.rds  最終モデル (係数・λ・標準化係数。外部検証と eTable 3 に使う)
#        output/derived/perf_dev.rds      optimism 補正 AUC・slope・CITL と 95% CI、ΔAUC (E5・E7・E8)、
#                                         構成要素ごとの判別能 (E9: comp・comp_delta)
#        output/derived/calib_dev.rds     optimism 補正の較正曲線 (健診モデル。Figure 2)
#        output/derived/pred_hist_dev.rds 予測確率の分布 (Figure 2 下部のヒストグラム)
#        output/derived/boot_dist.rds     bootstrap での λ と calibration slope の分布 (eTable 4・内部診断)
#        output/derived/e4_dev_apparent.rds 高リスク者の抽出の開発での見かけの値 (eTable 6)
#        output/derived/cache/boot_dev_*.rds bootstrap 各回の指標 (キャッシュ)
# 対応 : protocol §7.3・§7.5 (primary, E1, E2, E5–E9 の開発側)・§7.13 (Figure 1–3, eTable 3, 4, 8)
# 時間 : 本番 (N_BOOT = 500) で 20 コア (Intel Core Ultra 7 165H) なら 1 アウトカム約 20 分、計 70〜80 分。
#        TEST_MODE では数分。
#        2 回目以降は、設定とデータが同じならキャッシュを読むだけで数秒
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
as_sets <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))
D <- as_sets$D
message("N_BOOT = ", N_BOOT, "、コア数 = ", N_CORES)

# 比べるモデルの組 (ΔAUC = model1 − model2, §3.4・§7.5)
DELTA_PAIRS <- tribble(
  ~contrast,    ~model1,          ~model2,
  "E5",         "direct",         "checkup",        # 健診前 HRQOL − 健診
  "E7",         "checkup_direct", "checkup",        # 健診 + 健診前 HRQOL − 健診
  "E7_ref",     "checkup_direct", "direct",         # 健診 + 健診前 HRQOL − 健診前 HRQOL (参考, eTable 7)
  "E8",         "checkup",        "questionnaire"   # 健診 − 質問票
)
HIST_BREAKS <- seq(0, 1, by = 0.02)   # 予測確率のヒストグラムの区切り (2% 幅)
# 構成要素ごとの判別能 (E9): 約 1 年半後だけ、健診・健診前 HRQOL・統合の 3 モデル (§7.5)
COMP_MODELS <- c("checkup", "direct", "checkup_direct")

results <- list()
for (k in seq_len(nrow(OUTCOMES))) {
  oc <- OUTCOMES[k, ]
  message("\n==== ", oc$y, " ====")

  # ---- 1. 解析対象 (結果が確定した人) ----
  # 結果不明 (y が NA) の人は complete case として除く (§7.8)
  d <- D |> filter(!is.na(.data[[oc$y]]))
  y <- d[[oc$y]]

  # ---- 2・3. 最終モデルの推定と bootstrap 内部検証 (R/fn_boot.R の run_dev_bootstrap) ----
  # 最終モデル: 開発データ全体で λ を選んで推定 (CV 分割は全モデル共通、乱数種 SEED + 100 × 番号)
  # bootstrap : N_BOOT 回。標本と fold は先にまとめて作る (乱数種 SEED + 番号)。並列に計算し、キャッシュする
  # 約 1 年半後は、同じ bootstrap 標本で構成要素ごとの AUC も計算する (E9)
  comps <- if (oc$time == "18m") make_components(d, oc$domain, oc$time) else NULL
  res <- run_dev_bootstrap(d, y, names(MODELS), seed_final = SEED + 100 * k, seed_boot = SEED + k,
                           cache_name = paste0("boot_dev_", oc$y), curve_models = "checkup",
                           components = comps, comp_models = COMP_MODELS)
  final <- res$final; orig <- res$orig; grid <- res$grid
  boot_metrics <- res$metrics; boot_curves <- res$curves

  # ---- 4. optimism 補正した性能と ΔAUC ----
  perf  <- summarise_optimism(orig, boot_metrics) |> mutate(outcome = oc$y, n = length(y), events = sum(y))
  delta <- summarise_delta(orig, boot_metrics, DELTA_PAIRS) |> mutate(outcome = oc$y)
  # 構成要素ごとの判別能と ΔAUC (E9。E5・E7 の組だけ)
  comp <- if (is.null(comps)) NULL else
    summarise_components(final, comps, res$comp, filter(DELTA_PAIRS, contrast %in% c("E5", "E7")))

  # ---- 5. optimism 補正の較正曲線 (健診モデル, E2) ----
  # 補正曲線 = 最終モデルの見かけの曲線 − 平均の optimism (格子点ごと)
  app_curve <- calib_curve(y, plogis(final$checkup$lp))
  opt_curve <- boot_curves |>
    filter(model == "checkup") |>
    group_by(p) |>
    summarise(optimism = mean(app - test, na.rm = TRUE), .groups = "drop")
  calib <- tibble(outcome = oc$y, model = "checkup", p = CALIB_GRID, apparent = app_curve) |>
    left_join(opt_curve, by = "p") |>
    mutate(corrected = pmin(pmax(apparent - optimism, 0), 1))

  # ---- 6. 予測確率の分布 (ヒストグラムの度数。個人の値は保存しない) ----
  pred_hist <- map_dfr(names(final), \(m) {
    h <- hist(plogis(final[[m]]$lp), breaks = HIST_BREAKS, plot = FALSE)
    tibble(outcome = oc$y, model = m, mid = h$mids, count = h$counts)
  })

  # ---- 7. bootstrap での λ と calibration slope の分布 (§7.5) ----
  dist <- boot_metrics |>
    mutate(outcome = oc$y,
           # 選ばれた λ が候補列の端かどうか。
           #   最大の端 = 罰則が最も強い (ほぼ切片だけのモデル)。候補の範囲が足りない可能性がある
           #   最小の端 = 罰則がほとんど効いていない (罰則なしの推定に近い)。予測因子の少ないモデルで起こりうる
           lambda_at_max = lambda_index == 1,
           lambda_at_min = lambda_index == lengths(grid)[model])
  # 最終モデルは、個人ごとの予測値 (lp) を除いて保存する (個人単位のデータを derived に置かないため)
  results[[oc$y]] <- list(final = map(final, \(f) f[names(f) != "lp"]), perf = perf, delta = delta,
                          comp = if (!is.null(comp)) mutate(comp$perf, outcome = oc$y),
                          comp_delta = if (!is.null(comp)) mutate(comp$delta, outcome = oc$y),
                          calib = calib, pred_hist = pred_hist, dist = dist)
  print(perf |> filter(stat == "auc") |> select(model, apparent, optimism, corrected, lo, hi))
}

# ---- 8. 高リスク者の抽出: 開発での見かけの値 (E4, eTable 6 の参考値) ----
# 全適格者 (結果不明を含む 1,478 人) に最終モデルを当てはめ、上位 10・20・30% を抽出する。
# 補足評価として、健診前の HRQOL が低くなかった人 (PCS 複合は健診前 PCS >= 40、MCS 複合は MCS >= 40) に限った値も求める
E4_FRACS <- c(0.1, 0.2, 0.3)
e4_dev <- map_dfr(c("y_pcs_18m", "y_mcs_18m"), \(yv) {
  dom <- if (str_detect(yv, "pcs")) "pcs" else "mcs"
  map_dfr(c("checkup", "direct"), \(m) {
    f <- results[[yv]]$final[[m]]
    p_all <- plogis(predict_lp(f$coef, make_x(D, f$vars)))
    map_dfr(c("all", "pre_not_low"), \(grp) {
      keep <- if (grp == "all") rep(TRUE, nrow(D)) else D[[paste0(dom, "_pre")]] >= LOW_QOL_CUTOFF
      map_dfr(E4_FRACS, \(fr) as_tibble_row(highrisk_stats(p_all[keep], D[[yv]][keep], fr)) |>
                mutate(outcome = yv, model = m, group = grp, frac = fr))
    })
  })
})

# ---- 9. 保存 ----
save_derived(map(results, "final"), "final_models")
save_derived(list(perf = bind_rows(map(results, "perf")), delta = bind_rows(map(results, "delta")),
                  comp = bind_rows(map(results, "comp")), comp_delta = bind_rows(map(results, "comp_delta"))),
             "perf_dev")
save_derived(bind_rows(map(results, "calib")), "calib_dev")
save_derived(bind_rows(map(results, "pred_hist")), "pred_hist_dev")
save_derived(bind_rows(map(results, "dist")), "boot_dist")
save_derived(e4_dev, "e4_dev_apparent")
message("03 完了")
