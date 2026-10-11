# =============================================================================
# 09_recalibration.R  外部検証での再較正
# -----------------------------------------------------------------------------
# 目的 : 健診ベースモデル (約 1 年半後、最終モデル) の外部検証 (V) で calibration slope が 1 を
#        超えたため、ロジスティック再較正 (切片と傾きの更新) を行い、再較正後の較正を示す。
#        - 再較正の係数: V の結果確定者全員で logit(p) = a + b × LP を推定 (a・b と Wald 95% CI)。
#          利用者がモデルを当てはめるときの更新後の式として示す
#        - 再較正後の較正: 同じデータで推定・評価すると slope = 1・CITL = 0 に自明になるため、
#          V の中の 10 分割交差検証で評価する (9 分割で a・b を推定し、残り 1 分割に当てはめる)。
#          交差検証で得た再較正後の予測で、slope・CITL (全体・性別) と較正曲線を求める
#        判別能 (AUC) は単調な変換で変わらないため示さない
# 入力 : data/processed/analysis_sets.rds (01), output/derived/final_models.rds (03)
# 出力 : output/derived/recal.rds  list(coef = 再較正の係数, perf = 再較正前後の slope・CITL, calib = 較正曲線)
# 対応 : protocol §7.6・§7.13 (eTable 5)
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
K <- 10
set.seed(SEED + 9000)   # 03・04・05・08 と重ならない値
coef_rows <- list(); perf_rows <- list(); calib_rows <- list()
for (dom in c("pcs", "mcs")) {
  yv <- paste0("y_", dom, "_18m")
  d  <- V |> filter(!is.na(.data[[yv]]))
  y  <- d[[yv]]
  lp <- lp_of(d, yv, "checkup")

  # 1. V 全体での再較正の係数 (a: 切片, b: 傾き)
  f  <- glm(y ~ lp, family = binomial())
  ci <- suppressMessages(confint.default(f))
  coef_rows[[dom]] <- tibble(outcome = yv, term = c("intercept", "slope"),
                             estimate = unname(coef(f)), lo = ci[, 1], hi = ci[, 2],
                             n = length(y), events = sum(y))

  # 2. 10 分割交差検証で再較正後の予測を作る (年度とアウトカムで層化して fold を割り当てる)
  fold <- integer(length(y))
  for (s in split(seq_along(y), interaction(d$val_fy, y, drop = TRUE))) {
    fold[s] <- sample(rep_len(seq_len(K), length(s)))
  }
  lp_cv <- numeric(length(y))
  for (k in seq_len(K)) {
    tr <- fold != k
    fk <- glm(y[tr] ~ lp[tr], family = binomial())
    lp_cv[!tr] <- coef(fk)[1] + coef(fk)[2] * lp[!tr]
  }

  # 3. 再較正前後の slope・CITL (全体と性別)
  for (grp in c("All", levels(d$sex))) {
    k <- if (grp == "All") rep(TRUE, length(y)) else d$sex == grp
    perf_rows[[length(perf_rows) + 1]] <- bind_rows(
      ext_perf(y[k], lp[k]) |> mutate(version = "Original"),
      ext_perf(y[k], lp_cv[k]) |> mutate(version = "Recalibrated (cross-validated)")) |>
      filter(stat != "auc") |>
      mutate(outcome = yv, group = grp)
  }
  # 4. 較正曲線 (全体)
  calib_rows[[dom]] <- bind_rows(
    tibble(version = "Original", p = CALIB_GRID, observed = calib_curve(y, plogis(lp))),
    tibble(version = "Recalibrated (cross-validated)", p = CALIB_GRID, observed = calib_curve(y, plogis(lp_cv)))) |>
    mutate(outcome = yv)
}
recal <- list(coef = bind_rows(coef_rows), perf = bind_rows(perf_rows), calib = bind_rows(calib_rows))
print(recal$coef)
print(recal$perf |> select(outcome, group, version, stat, estimate, lo, hi))
save_derived(recal, "recal")
message("09 完了")
