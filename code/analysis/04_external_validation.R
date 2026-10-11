# =============================================================================
# 04_external_validation.R  時間的外部検証と高リスク者の抽出
# -----------------------------------------------------------------------------
# 目的 : 03 で開発した最終モデル (係数を固定) を、開発に入っていない後年の参加者に当てはめて
#        性能を評価する (E3・E6 の外部検証、E5・E7・E8 の外部検証側、S4)。
#        あわせて、予測リスクの上位 10・20・30% を抽出したときの感度と陽性的中率を求める (E4)。
# 入力 : data/processed/analysis_sets.rds (01), output/derived/final_models.rds (03)
# 出力 : output/derived/perf_ext.rds     AUC (DeLong CI)・slope・CITL、ΔAUC (対応のある DeLong)、
#                                        構成要素ごとの判別能 (E9: comp・comp_delta)、換算したリスクの較正
#                                        (E9: comp_k・comp_cal・comp_calib)
#        output/derived/calib_ext.rds    外部検証の較正曲線 (Figure 2 の破線)
#        output/derived/pred_hist_ext.rds 予測確率の分布 (Figure 2 下部)
#        output/derived/tbl2_highrisk.rds 高リスク者の抽出 (Table 2)
#        output/derived/roc_curves.rds   ROC 曲線の座標 (Figure 1・3。開発は見かけ、外部検証は V・V_6m)
# 対応 : protocol §7.1・§7.3・§7.5 (E3, E4, E5–E9 の外部検証)・§7.7 S4・§7.13 (Figure 1–3, Table 2, eTable 4・eTable 7・eTable 8)
# 時間 : 1 分程度 (E4 の bootstrap を含む)
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
as_sets <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))
final   <- read_derived("final_models")   # final[[アウトカム]][[モデル]]

DELTA_PAIRS <- tribble(
  ~contrast, ~model1,          ~model2,
  "E5",      "direct",         "checkup",
  "E7",      "checkup_direct", "checkup",
  "E7_ref",  "checkup_direct", "direct",
  "E8",      "checkup",        "questionnaire"
)
HIST_BREAKS <- seq(0, 1, by = 0.02)

# 最終モデルを当てはめて線形予測子を計算する (係数は開発データのまま。再推定しない, §7.3)
lp_of <- function(d, yv, m) {
  f <- final[[yv]][[m]]
  x <- make_x(d, f$vars)
  stopifnot(identical(colnames(x), f$x_colnames))   # 開発と同じ列が同じ順に並んでいること
  predict_lp(f$coef, x)
}

# ---- 1. 評価する集合の一覧 ----
# 約 1 年半後のモデルは V (と年度別・S4) に、約半年後のモデルは V_6m に当てはめる
# (同じモデルを別の時点には当てはめない, §7.3)
V <- as_sets$V; S4 <- as_sets$S4
eval_sets <- list(
  list(set = "V",        data = V,                           time = "18m"),
  list(set = "V_FY2022", data = filter(V, val_fy == "FY2022"), time = "18m"),
  list(set = "V_FY2023", data = filter(V, val_fy == "FY2023"), time = "18m"),
  list(set = "S4_FY2022", data = filter(S4, val_fy == "FY2022"), time = "18m"),
  list(set = "S4_FY2023", data = filter(S4, val_fy == "FY2023"), time = "18m"),
  list(set = "V_6m",     data = as_sets$V6m,                 time = "6m")
)

# ROC 曲線の座標 (偽陽性率・感度) と、その集団での AUC
roc_df <- function(y, lp) {
  r <- pROC::roc(y, lp, levels = c(0, 1), direction = "<", quiet = TRUE)
  tibble(fpr = 1 - r$specificities, tpr = r$sensitivities, auc_curve = as.numeric(pROC::auc(r))) |>
    arrange(fpr, tpr)
}

perf <- list(); delta <- list(); calib <- list(); pred_hist <- list(); roc_rows <- list()
for (es in eval_sets) {
  for (dom in c("pcs", "mcs")) {
    yv <- paste0("y_", dom, "_", es$time)
    d  <- es$data |> filter(!is.na(.data[[yv]]))     # 結果が確定した人で評価する
    y  <- d[[yv]]
    lps <- map(set_names(names(MODELS)), \(m) lp_of(d, yv, m))

    # ---- 2. AUC・較正 (モデルごと) ----
    perf[[length(perf) + 1]] <- imap_dfr(lps, \(lp, m) ext_perf(y, lp) |> mutate(model = m)) |>
      mutate(set = es$set, outcome = yv)

    # ---- 3. ΔAUC (対応のある DeLong 法) ----
    delta[[length(delta) + 1]] <- map_dfr(seq_len(nrow(DELTA_PAIRS)), \(i)
      ext_delta(y, lps[[DELTA_PAIRS$model1[i]]], lps[[DELTA_PAIRS$model2[i]]]) |>
        mutate(contrast = DELTA_PAIRS$contrast[i], model1 = DELTA_PAIRS$model1[i],
               model2 = DELTA_PAIRS$model2[i])) |>
      mutate(set = es$set, outcome = yv)

    # ---- 4. 較正曲線と予測確率の分布 (主たる集合 V・V_6m のみ) ----
    if (es$set %in% c("V", "V_6m")) {
      calib[[length(calib) + 1]] <- imap_dfr(lps, \(lp, m)
        tibble(model = m, p = CALIB_GRID, observed = calib_curve(y, plogis(lp)))) |>
        mutate(set = es$set, outcome = yv)
      pred_hist[[length(pred_hist) + 1]] <- imap_dfr(lps, \(lp, m) {
        h <- hist(plogis(lp), breaks = HIST_BREAKS, plot = FALSE)
        tibble(model = m, mid = h$mids, count = h$counts)
      }) |> mutate(set = es$set, outcome = yv)
      # ROC 曲線 (Figure 1 の破線・Figure 3)
      roc_rows[[length(roc_rows) + 1]] <- imap_dfr(lps, \(lp, m) roc_df(y, lp) |> mutate(model = m)) |>
        mutate(cohort = "Temporal validation", set = es$set, outcome = yv)
    }
  }
}
perf <- bind_rows(perf); delta <- bind_rows(delta)
print(perf |> filter(set %in% c("V", "V_6m"), stat == "auc", model == "checkup"))

# ---- 4b. 開発の ROC 曲線 (Figure 1 の実線。健診ベースモデル・約 1 年半後) ----
# 最終モデルを開発データに当てはめた見かけの曲線 (optimism 補正した曲線は作れないため)
for (dom in c("pcs", "mcs")) {
  yv <- paste0("y_", dom, "_18m")
  d  <- as_sets$D |> filter(!is.na(.data[[yv]]))
  roc_rows[[length(roc_rows) + 1]] <- roc_df(d[[yv]], lp_of(d, yv, "checkup")) |>
    mutate(model = "checkup", cohort = "Development", set = "D", outcome = yv)
}
roc_curves <- bind_rows(roc_rows)

# ---- 4c. 構成要素ごとの判別能 (E9, eTable 8) ----
# 約 1 年半後の PCS 複合・MCS 複合の最終モデル (健診・健診前 HRQOL・統合) を、V (年度を統合) の
# 複合アウトカムの解析対象の中で、構成要素ごとの対象に当てはめる (§7.5 E9)。
# 較正は、予測リスクを Gondrie らの方法で構成要素のリスクに換算して評価する [PMID 22658619]:
#   換算係数 k = 構成要素の発生割合 / 複合アウトカムの発生割合 (開発データで求める)。原法どおり、どちらの
#   発生割合も結果が確定した人の全員を分母とし、構成要素は実際に起きた数で数える (他の構成要素との重なりを含む)
#   換算したリスク = 1 - (1 - p)^k (原法は Cox のベースライン累積ハザードに k を掛ける。p を累積ハザード
#   -log(1 - p) とみなしたロジスティックモデルでの近似)。AUC は単調な変換で変わらない
#   換算したリスクはコホート全体での構成要素のリスクであり、(a) の条件つきの対象のリスクではない。そのため
#   較正は、構成要素ごとに結果が確定した人の全員で評価する (低い HRQOL: SF-8 の値が 40 未満 = イベント、
#   それ以外 [要介護 3 以上・死亡で SF-8 がない人を含む] = 非イベント)。判別能 (AUC) は make_components() の対象で評価する
COMP_MODELS <- c("checkup", "direct", "checkup_direct")
adapt_risk <- function(p, k) 1 - (1 - p)^k
# 較正を評価する構成要素 (結果が確定した人の全員が対象)
cal_components <- function(d, dom, tag) {
  v <- d[[paste0(dom, "_", tag)]]
  list(low_hrqol  = list(in_pop = rep(TRUE, nrow(d)), y = as.integer(!is.na(v) & v < LOW_QOL_CUTOFF)),
       ltc3_death = make_components(d, dom, tag)$ltc3_death)
}
comp_k <- map_dfr(c("pcs", "mcs"), \(dom) {
  yv <- paste0("y_", dom, "_18m")
  dd <- as_sets$D |> filter(!is.na(.data[[yv]]))
  cc <- cal_components(dd, dom, "18m")
  imap_dfr(cc, \(c, cn) tibble(outcome = yv, component = cn,
                               inc_component = mean(c$y[c$in_pop]), inc_composite = mean(dd[[yv]]),
                               k = inc_component / inc_composite))
})
print(comp_k)
comp_rows <- list(); comp_delta_rows <- list(); comp_cal_rows <- list(); comp_calib_rows <- list()
for (dom in c("pcs", "mcs")) {
  yv <- paste0("y_", dom, "_18m")
  d  <- V |> filter(!is.na(.data[[yv]]))
  comps <- make_components(d, dom, "18m")
  cals  <- cal_components(d, dom, "18m")
  lps <- map(set_names(COMP_MODELS), \(m) lp_of(d, yv, m))
  for (cn in names(comps)) {
    keep <- comps[[cn]]$in_pop
    yc   <- comps[[cn]]$y[keep]
    comp_rows[[length(comp_rows) + 1]] <- imap_dfr(lps, \(lp, m)
      ext_perf(yc, lp[keep]) |> filter(stat == "auc") |> mutate(model = m)) |>
      mutate(component = cn, set = "V", outcome = yv)
    comp_delta_rows[[length(comp_delta_rows) + 1]] <- DELTA_PAIRS |>
      filter(contrast %in% c("E5", "E7")) |>
      pmap_dfr(\(contrast, model1, model2)
        ext_delta(yc, lps[[model1]][keep], lps[[model2]][keep]) |>
          mutate(contrast = contrast, model1 = model1, model2 = model2)) |>
      mutate(component = cn, set = "V", outcome = yv)
    # 換算前 (複合アウトカムの予測リスクをそのまま) と換算後の較正 (calibration slope・CITL と較正曲線)。
    # 較正の対象は結果が確定した人の全員 (cal_components)
    k    <- comp_k$k[comp_k$outcome == yv & comp_k$component == cn]
    keep <- cals[[cn]]$in_pop
    yc   <- cals[[cn]]$y[keep]
    for (m in COMP_MODELS) {
      p_raw <- plogis(lps[[m]][keep])
      p_ad  <- adapt_risk(p_raw, k)
      comp_cal_rows[[length(comp_cal_rows) + 1]] <- bind_rows(
        ext_perf(yc, qlogis(p_raw)) |> mutate(version = "unadjusted"),
        ext_perf(yc, qlogis(p_ad)) |> mutate(version = "adapted")) |>
        filter(stat != "auc") |>
        mutate(model = m, component = cn, k = k, set = "V", outcome = yv)
      comp_calib_rows[[length(comp_calib_rows) + 1]] <- bind_rows(
        tibble(version = "unadjusted", p = CALIB_GRID, observed = calib_curve(yc, p_raw)),
        tibble(version = "adapted",    p = CALIB_GRID, observed = calib_curve(yc, p_ad))) |>
        mutate(model = m, component = cn, set = "V", outcome = yv)
    }
  }
}
comp_ext <- bind_rows(comp_rows); comp_delta_ext <- bind_rows(comp_delta_rows)
comp_cal_ext <- bind_rows(comp_cal_rows); comp_calib_ext <- bind_rows(comp_calib_rows)
print(comp_cal_ext |> filter(model == "checkup") |> select(outcome, component, version, stat, estimate, lo, hi))
print(comp_ext |> select(outcome, component, model, estimate, lo, hi, n, events))

# ---- 5. 高リスク者の抽出 (E4, Table 2) ----
# V の全適格者 (結果不明を含む) に最終モデルを当てはめ、予測リスクの順位で上位 10・20・30% を抽出する。
# 閾値は全適格者で決め (結果確定者だけで決め直さない)、感度と陽性的中率は結果確定者で計算する (§7.5 E4)。
# 95% CI は、年度 (FY2022 / FY2023) で層化して個人単位に再標本化する bootstrap のパーセンタイル法。
# 各標本で抽出の閾値を計算し直す (モデルの係数は固定)
E4_FRACS <- c(0.1, 0.2, 0.3)
e4_rows <- list()
set.seed(SEED + 4000)   # 03 (SEED + 1〜4, + 100〜400) や 05 と重ならない値
strata <- split(seq_len(nrow(V)), V$val_fy)
boot_idx <- replicate(N_BOOT_E4, unlist(lapply(strata, \(ix) ix[sample.int(length(ix), replace = TRUE)])),
                      simplify = FALSE)
for (dom in c("pcs", "mcs")) {
  yv <- paste0("y_", dom, "_18m")
  for (m in c("checkup", "direct")) {
    p_all <- plogis(lp_of(V, yv, m))
    for (grp in c("all", "pre_not_low")) {
      # 補足評価: 健診前の調査で HRQOL が低くなかった人 (PCS 複合は PCS >= 40、MCS 複合は MCS >= 40)。
      # この集団の中で順位を定める
      keep <- if (grp == "all") rep(TRUE, nrow(V)) else V[[paste0(dom, "_pre")]] >= LOW_QOL_CUTOFF
      for (fr in E4_FRACS) {
        est <- highrisk_stats(p_all[keep], V[[yv]][keep], fr)
        bs <- t(vapply(boot_idx, \(ix) {
          ix <- ix[keep[ix]]
          highrisk_stats(p_all[ix], V[[yv]][ix], fr)[c("sensitivity", "ppv")]
        }, numeric(2)))
        e4_rows[[length(e4_rows) + 1]] <- as_tibble_row(est) |>
          mutate(outcome = yv, model = m, group = grp, frac = fr,
                 sens_lo = quantile(bs[, "sensitivity"], 0.025, na.rm = TRUE),
                 sens_hi = quantile(bs[, "sensitivity"], 0.975, na.rm = TRUE),
                 ppv_lo  = quantile(bs[, "ppv"], 0.025, na.rm = TRUE),
                 ppv_hi  = quantile(bs[, "ppv"], 0.975, na.rm = TRUE),
                 n_boot = N_BOOT_E4)
      }
    }
  }
}
tbl2 <- bind_rows(e4_rows)
print(tbl2 |> filter(group == "all") |> select(outcome, model, frac, sensitivity, ppv, prevalence))

# ---- 6. 保存 ----
save_derived(list(perf = perf, delta = delta, comp = comp_ext, comp_delta = comp_delta_ext,
                  comp_k = comp_k, comp_cal = comp_cal_ext, comp_calib = comp_calib_ext), "perf_ext")
save_derived(bind_rows(calib), "calib_ext")
save_derived(bind_rows(pred_hist), "pred_hist_ext")
save_derived(tbl2, "tbl2_highrisk")
save_derived(roc_curves, "roc_curves")
message("04 完了")
