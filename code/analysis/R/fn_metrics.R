# =============================================================================
# fn_metrics.R  予測性能の指標 (AUC・較正・較正曲線・高リスク者の抽出)
# -----------------------------------------------------------------------------
# bootstrap の中で何千回も呼ぶため、どれも軽い計算で書いている。
# 外部検証の 95% CI (DeLong 法) だけは pROC を使う (04)。
# =============================================================================

# ---- AUC (C 統計量) ----
# 順位を使う式 (Mann–Whitney の U 統計量と同じ)。pROC::auc と同じ値になるが、はるかに速い。
#   AUC = (イベントの人の予測値の順位の合計 − n1(n1+1)/2) / (n1 × n0)
# y: 0/1、score: 予測確率または線形予測子 (順位しか使わないのでどちらでもよい)
auc_fast <- function(y, score) {
  r <- rank(score)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# ---- 較正 (calibration slope・calibration-in-the-large) ----
# slope: アウトカムを線形予測子 lp に回帰したときの傾き。1 が理想。
#        1 より小さい = 予測が両極に寄りすぎ (過学習)、1 より大きい = 予測が控えめすぎ
# CITL : lp をオフセット (係数を 1 に固定) にして切片だけを推定した値。0 が理想。
#        正 = 予測が全体に低すぎる、負 = 高すぎる
# glm() より軽い glm.fit() を直接呼ぶ
calib_stats <- function(y, lp, weights = NULL) {
  if (is.null(weights)) weights <- rep(1, length(y))
  slope <- suppressWarnings(glm.fit(cbind(1, lp), y, weights = weights, family = binomial()))$coefficients[2]
  citl  <- suppressWarnings(glm.fit(matrix(1, length(y)), y, weights = weights, offset = lp,
                                    family = binomial()))$coefficients[1]
  c(slope = unname(slope), citl = unname(citl))
}

# 3 つの指標をまとめて返す
perf_stats <- function(y, lp) c(auc = auc_fast(y, lp), calib_stats(y, lp))

# ---- 較正曲線 ----
# 予測確率 p に対する、実際のイベント割合を loess (局所回帰) でなめらかに推定し、
# 決めた格子点 (grid) での値を返す。格子点が予測確率の範囲の外なら NA。
CALIB_GRID <- seq(0.01, 0.80, by = 0.01)   # 図の横軸 (0–80%, §7.13 (d)) に合わせる
calib_curve <- function(y, p, grid = CALIB_GRID, weights = NULL) {
  fit <- suppressWarnings(loess(y ~ p, weights = weights, span = 0.75, degree = 1,
                                family = "gaussian"))
  out <- suppressWarnings(predict(fit, newdata = data.frame(p = grid)))
  out[grid < min(p) | grid > max(p)] <- NA
  pmin(pmax(out, 0), 1)
}

# ---- 高リスク者の抽出 (E4) ----
# 予測リスクの高い順に上位 frac (例 0.1) を抽出し、感度と陽性的中率を求める (§7.5 E4)。
#   - 順位は、結果が観察できたかどうかにかかわらず「全適格者」で決める (健診の場を模擬)
#   - 閾値と同じ予測値の人はすべて含める (実際の抽出割合を併記する)
#   - 感度と陽性的中率は、結果が確定した人 (y が NA でない人) で計算する
# p: 予測確率 (全適格者)、y: アウトカム (NA = 結果不明)、w: 重み (S1 の IPW。既定は 1)
highrisk_stats <- function(p, y, frac, w = NULL) {
  if (is.null(w)) w <- rep(1, length(p))
  thr <- sort(p, decreasing = TRUE)[ceiling(frac * length(p))]
  sel <- p >= thr
  known <- !is.na(y)
  ev <- known & y == 1
  c(threshold = thr,
    selected_prop = mean(sel),
    sensitivity = sum(w[sel & ev]) / sum(w[ev]),
    ppv = sum(w[sel & ev]) / sum(w[sel & known]),
    prevalence = sum(w[ev]) / sum(w[known]),                 # 無作為に抽出したときの陽性的中率
    n_sel = sum(sel), n_sel_known = sum(sel & known), n_sel_unknown = sum(sel & !known),
    n_unsel = sum(!sel), n_unsel_known = sum(!sel & known), n_unsel_unknown = sum(!sel & !known),
    ev_sel = sum(sel & ev), ev_total = sum(ev))
}

# ---- 外部検証の性能と 95% CI (§7.5 E3) ----
# 係数を固定したモデルの線形予測子 lp を、検証データに当てはめて評価する。
#   AUC   : DeLong 法の 95% CI (pROC)
#   slope・CITL: ロジスティック回帰の標準誤差による Wald の 95% CI
ext_perf <- function(y, lp) {
  roc <- pROC::roc(y, lp, levels = c(0, 1), direction = "<", quiet = TRUE)
  ci  <- as.numeric(pROC::ci.auc(roc, method = "delong"))
  f_slope <- glm(y ~ lp, family = binomial())
  f_citl  <- glm(y ~ 1, offset = lp, family = binomial())
  se_s <- sqrt(diag(vcov(f_slope)))[2]; se_c <- sqrt(diag(vcov(f_citl)))[1]
  b_s <- coef(f_slope)[2]; b_c <- coef(f_citl)[1]
  tibble(stat = c("auc", "slope", "citl"),
         estimate = c(ci[2], b_s, b_c),
         lo = c(ci[1], b_s - 1.96 * se_s, b_c - 1.96 * se_c),
         hi = c(ci[3], b_s + 1.96 * se_s, b_c + 1.96 * se_c),
         n = length(y), events = sum(y))
}

# ΔAUC (lp1 のモデル − lp2 のモデル) と、対応のある DeLong 法の 95% CI (§7.5)
ext_delta <- function(y, lp1, lp2) {
  r1 <- pROC::roc(y, lp1, levels = c(0, 1), direction = "<", quiet = TRUE)
  r2 <- pROC::roc(y, lp2, levels = c(0, 1), direction = "<", quiet = TRUE)
  tt <- pROC::roc.test(r1, r2, paired = TRUE, method = "delong")
  tibble(estimate = as.numeric(pROC::auc(r1) - pROC::auc(r2)),
         lo = tt$conf.int[1], hi = tt$conf.int[2])
}
