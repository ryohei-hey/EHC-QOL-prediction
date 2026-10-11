# =============================================================================
# 08_by_sex.R  主たる結果の性別ごとの提示
# -----------------------------------------------------------------------------
# 目的 : JAMA Network の投稿規定 (Reporting Sex and Gender: 主なアウトカムを可能な限り性別ごとに示す)
#        に合わせ、外部検証 (V、約 1 年半後) の主たる結果を性別ごとに示す。記述的な提示であり、
#性別間の差の検定や交互作用の評価は行わない。
#        - 健診ベースモデル (最終モデル、係数固定) の AUC (DeLong 95% CI)・calibration slope・CITL
#        - 高リスク者の抽出 (E4): 抽出の閾値は V の全適格者 (男女合わせて) で決め (健診の場での抽出を
#          模擬するため性別ごとには決め直さない)、感度と陽性的中率を性別ごとに結果確定者で計算する。
#          95% CI は 04 と同じく年度で層化した個人単位の bootstrap (各標本で全体の閾値を計算し直す)
# 入力 : data/processed/analysis_sets.rds (01), output/derived/final_models.rds (03)
# 出力 : output/derived/by_sex.rds  list(perf = 性別ごとの AUC・較正, e4 = 性別ごとの感度・陽性的中率)
# 対応 : JAMA Intern Med の投稿規定 (protocol §7.6・§7.13)
# 時間 : 1 分程度
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
as_sets <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))
final   <- read_derived("final_models")

lp_of <- function(d, yv, m) {
  f <- final[[yv]][[m]]
  x <- make_x(d, f$vars)
  stopifnot(identical(colnames(x), f$x_colnames))
  predict_lp(f$coef, x)
}

V <- as_sets$V
SEXES <- levels(V$sex)   # Female, Male

# ---- 1. AUC・較正 (結果が確定した人) ----
perf <- map_dfr(c("pcs", "mcs"), \(dom) {
  yv <- paste0("y_", dom, "_18m")
  d  <- V |> filter(!is.na(.data[[yv]]))
  lp <- lp_of(d, yv, "checkup")
  map_dfr(SEXES, \(s) {
    k <- d$sex == s
    ext_perf(d[[yv]][k], lp[k]) |> mutate(sex = s)
  }) |> mutate(outcome = yv, model = "checkup", set = "V")
})
print(perf |> filter(stat == "auc"))

# ---- 2. 高リスク者の抽出 (閾値は全体、集計は性別ごと) ----
# p・y・sex は V の全適格者 (結果不明を含む)。閾値は全員の予測リスクの順位で決める
highrisk_by_sex <- function(p, y, sex, frac, s) {
  thr   <- sort(p, decreasing = TRUE)[ceiling(frac * length(p))]
  sel   <- p >= thr
  grp   <- sex == s
  known <- grp & !is.na(y)
  ev    <- known & y == 1
  c(sensitivity = sum(sel & ev) / sum(ev),
    ppv = sum(sel & ev) / sum(sel & known),
    prevalence = sum(ev) / sum(known),
    n_group = sum(grp), n_known = sum(known), ev_total = sum(ev),
    n_sel = sum(sel & grp), n_sel_known = sum(sel & known), ev_sel = sum(sel & ev))
}

E4_FRACS <- c(0.1, 0.2, 0.3)
set.seed(SEED + 8000)   # 03・04・05 と重ならない値
strata   <- split(seq_len(nrow(V)), V$val_fy)
boot_idx <- replicate(N_BOOT_E4, unlist(lapply(strata, \(ix) ix[sample.int(length(ix), replace = TRUE)])),
                      simplify = FALSE)
e4 <- list()
for (dom in c("pcs", "mcs")) {
  yv <- paste0("y_", dom, "_18m")
  p_all <- plogis(lp_of(V, yv, "checkup"))
  for (s in SEXES) {
    for (fr in E4_FRACS) {
      est <- highrisk_by_sex(p_all, V[[yv]], V$sex, fr, s)
      bs <- t(vapply(boot_idx, \(ix)
        highrisk_by_sex(p_all[ix], V[[yv]][ix], V$sex[ix], fr, s)[c("sensitivity", "ppv")],
        numeric(2)))
      e4[[length(e4) + 1]] <- as_tibble_row(est) |>
        mutate(outcome = yv, model = "checkup", sex = s, frac = fr,
               sens_lo = quantile(bs[, "sensitivity"], 0.025, na.rm = TRUE),
               sens_hi = quantile(bs[, "sensitivity"], 0.975, na.rm = TRUE),
               ppv_lo  = quantile(bs[, "ppv"], 0.025, na.rm = TRUE),
               ppv_hi  = quantile(bs[, "ppv"], 0.975, na.rm = TRUE),
               n_boot = N_BOOT_E4)
    }
  }
}
e4 <- bind_rows(e4)
print(e4 |> select(outcome, sex, frac, sensitivity, ppv, prevalence, n_sel, n_known))

save_derived(list(perf = perf, e4 = e4), "by_sex")
message("08 完了")
