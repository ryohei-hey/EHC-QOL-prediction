# =============================================================================
# 05_sensitivity.R  感度分析 S1・S2・S3・S5 (S4 は 04 で計算済み)
# -----------------------------------------------------------------------------
# 目的 : 主解析の結論が、欠測の扱い・アウトカムの閾値・推定方法に左右されないかを確かめる (§7.7)。
#   S1: IPW (SF-8 の未回答を観察された変数で説明できる [MAR] と仮定した重みづけ)。開発と外部検証 (V)
#   S2: 結果不明者のうち、健診後に初めて要介護 1–2 の認定を受けた人を、低い HRQOL とみなしてイベントとする
#   S3: 低 HRQOL の閾値を PCS < 35、< 45 に変える
#   S5: 罰則なしのロジスティック回帰 + bootstrap の calibration slope による一様縮小
# 入力 : data/processed/analysis_sets.rds (01), output/derived/final_models.rds (03),
#        output/derived/perf_dev.rds (03), output/derived/perf_ext.rds (04)
# 出力 : output/derived/sens.rds  (eFigure 4 のデータ、S1 の開発・外部検証の値 [Figure 1・Table 2 の脚注]、
#                                  S2・S3・S5 の詳細、IPW の重みの要約 [内部診断])
#        output/derived/cache/boot_s2_*.rds, boot_s3_*.rds (bootstrap のキャッシュ)
# 対応 : protocol §7.7・§7.8・§7.13 (eFigure 4, Figure 1 と Table 2 の脚注)
# 時間 : 本番で 30〜40 分 (S2・S3 はそれぞれ 2 モデルの bootstrap を N_BOOT 回。S1 の bootstrap は 1,000 回)
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(WeightedROC))
as_sets  <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))
final    <- read_derived("final_models")
perf_dev <- read_derived("perf_dev")
perf_ext <- read_derived("perf_ext")
D <- as_sets$D; V <- as_sets$V
E4_FRACS <- c(0.1, 0.2, 0.3)
PAIR_E5 <- tibble(contrast = "E5", model1 = "direct", model2 = "checkup")

# 最終モデル (係数固定) の線形予測子
lp_of <- function(d, yv, m) predict_lp(final[[yv]][[m]]$coef, make_x(d, final[[yv]][[m]]$vars))

# =============================================================================
# S1: IPW (§7.7 S1)
# =============================================================================
# 重みの作り方:
#   - 台帳で結果が確定する人 (判定日までに要介護 3 以上または死亡) : 重み 1
#   - それ以外の人 (結果の確定に SF-8 の回答が必要な人。転出者を除く) のうち、回答者: 1 / 推定回答確率
#   - 未回答者・転出者: 重み 0 (解析に入らない)
# 回答確率は、台帳でイベントのない人 (転出者を除く) に限ったロジスティック回帰で推定する。
# 予測因子は年齢・性別・質問票 15 項目・健診項目・健診前の PCS/MCS。重みは 99 パーセンタイルで切り詰める。
# 安定化はしない (台帳で確定する人と回答者の相対的な重みを保つため)
IPW_VARS <- MODELS$checkup_direct
ipw_weights <- function(d, dom, tag) {
  y      <- d[[paste0("y_", dom, "_", tag)]]
  moved  <- d[[paste0("moved_", tag)]]
  ledger <- (d[[paste0("ltc3_", tag)]] | d[[paste0("death_", tag)]]) & !moved
  need   <- !ledger & !moved                 # 結果の確定に SF-8 の回答が必要な人
  resp   <- need & !is.na(y)                 # そのうち回答した人
  failed <- list(w = NULL, cap = NA_real_, n_trunc = NA_real_, n_need = sum(need), n_resp = sum(resp),
                 ok = FALSE)
  # カテゴリの水準を固定したデザイン行列で推定する (bootstrap 標本では、少ないカテゴリ
  # [例: 健康状態 Very poor] の人が推定の対象に 1 人もいないことがあり、formula の glm では予測できなくなるため)。
  # 推定できない係数 (その標本で値が一定の列) は 0 とする (glm が係数を NA にして除くのと同じ扱い)
  X <- cbind(1, make_x(d, IPW_VARS))
  fit <- tryCatch(suppressWarnings(glm.fit(X[need, , drop = FALSE], as.numeric(resp[need]),
                                            family = binomial())),
                  error = function(e) NULL)
  if (is.null(fit)) return(failed)           # 推定そのものがエラーになった回
  b <- fit$coefficients
  b[is.na(b)] <- 0
  p <- plogis(drop(X %*% b))
  # 推定が正常かの確認: 収束したか、回答者の回答確率が有限で 0 に張りついていないか。
  # 回答確率が 0 に近いと重み (1 / p) が極端に大きくなるので、その回は失敗として返す
  # (bootstrap では除き、件数を保存する)。回答確率が 1 に近い場合 (まれなカテゴリで全員が回答した
  # 標本などで起こる準分離) は重みがほぼ 1 になるだけで害がないので、除かない
  # (除くと、特定のカテゴリ構成の標本だけが系統的に除かれてしまう)
  if (!isTRUE(fit$converged) || !all(is.finite(p[resp])) || !all(p[resp] > 1e-8)) return(failed)
  w <- numeric(nrow(d))
  w[ledger] <- 1
  w[resp]   <- 1 / p[resp]
  cap <- unname(quantile(w[resp], 0.99))
  n_trunc <- sum(w[resp] > cap)
  w[resp] <- pmin(w[resp], cap)
  list(w = w, cap = cap, n_trunc = n_trunc, n_need = sum(need), n_resp = sum(resp),
       ok = all(is.finite(w)))
}

# 重みつきの AUC と較正 (y が NA の人は重み 0 なので除く)
weighted_perf <- function(y, lp, w) {
  k <- w > 0 & !is.na(y)
  auc <- WeightedAUC(WeightedROC(lp[k], y[k], w[k]))
  c(auc = auc, calib_stats(y[k], lp[k], weights = w[k]))
}

# 1 つの標本 (元のデータまたは bootstrap 標本) での S1 の値をまとめて計算する
# 戻り値: 名前つきベクトル (unw = 重みなし [結果確定者、見かけ]、ipw = 重みつき、ipw_ok = 重みが正常に作れたか)
# 重みが正常に作れなかった回は、重みに依存する値 (ipw・E4・cap・n_trunc) を NA にして返す
s1_eval <- function(d, dom, with_e4 = FALSE) {
  tag <- "18m"; yv <- paste0("y_", dom, "_", tag)
  y <- d[[yv]]
  lp_c <- lp_of(d, yv, "checkup"); lp_d <- lp_of(d, yv, "direct")
  w1 <- as.numeric(!is.na(y))                # 重みなし = 結果確定者に重み 1
  unw_c <- weighted_perf(y, lp_c, w1)
  out <- c(unw = unw_c, unw_d_e5 = weighted_perf(y, lp_d, w1)[["auc"]] - unw_c[["auc"]])

  # 重みに依存する値の名前 (値は NA で用意しておき、重みが正常なときだけ埋める)
  # (E4 の名前は、下で値を埋めるループと同じ入れ子の順で作る: "e4|モデル|集団|割合|指標")
  e4_names <- character(0)
  if (with_e4) {
    for (m in c("checkup", "direct")) for (grp in c("all", "pre_not_low")) for (fr in E4_FRACS)
      e4_names <- c(e4_names, paste("e4", m, grp, fr, c("sensitivity", "ppv"), sep = "|"))
  }
  ipw_part <- setNames(rep(NA_real_, 6 + length(e4_names)),
                       c("ipw.auc", "ipw.slope", "ipw.citl", "ipw_d_e5", "cap", "n_trunc", e4_names))

  wt <- tryCatch(ipw_weights(d, dom, tag), error = function(e) list(ok = FALSE))
  if (isTRUE(wt$ok)) {
    ipw_c <- weighted_perf(y, lp_c, wt$w)
    ipw_part[c("ipw.auc", "ipw.slope", "ipw.citl")] <- ipw_c
    ipw_part["ipw_d_e5"] <- weighted_perf(y, lp_d, wt$w)[["auc"]] - ipw_c[["auc"]]
    ipw_part[c("cap", "n_trunc")] <- c(wt$cap, wt$n_trunc)
    if (with_e4) {
      # 高リスク者の抽出 (E4) の重みつきの感度・陽性的中率。順位は全適格者 (または補足評価の集団) で決める
      for (m in c("checkup", "direct")) {
        p_all <- plogis(if (m == "checkup") lp_c else lp_d)
        for (grp in c("all", "pre_not_low")) {
          keep <- if (grp == "all") rep(TRUE, nrow(d)) else d[[paste0(dom, "_pre")]] >= LOW_QOL_CUTOFF
          for (fr in E4_FRACS) {
            hs <- highrisk_stats(p_all[keep], y[keep], fr, w = wt$w[keep])
            ipw_part[paste("e4", m, grp, fr, c("sensitivity", "ppv"), sep = "|")] <- hs[c("sensitivity", "ppv")]
          }
        }
      }
    }
  }
  c(out, ipw_part, ipw_ok = as.numeric(isTRUE(wt$ok)))
}

# bootstrap で 95% CI を求める (回答確率モデルの推定と切り詰めを標本ごとに繰り返す)
# strata: 層 (外部検証では年度) ごとに再標本化する
s1_boot <- function(d, dom, with_e4, strata, seed) {
  est <- s1_eval(d, dom, with_e4)
  # 元のデータで重みが作れなければ、bootstrap に進まずに止める
  if (!isTRUE(est[["ipw_ok"]] == 1)) stop("元のデータで回答確率モデルの推定が正常に終わりませんでした (S1)")
  set.seed(seed)
  groups <- split(seq_len(nrow(d)), strata)
  bs <- t(replicate(N_BOOT_IPW, {
    ix <- unlist(lapply(groups, \(g) g[sample.int(length(g), replace = TRUE)]))
    s1_eval(d[ix, ], dom, with_e4)
  }))
  stopifnot(identical(colnames(bs), names(est)))
  tibble(quantity = names(est), estimate = unname(est),
         lo = apply(bs, 2, quantile, 0.025, na.rm = TRUE),
         hi = apply(bs, 2, quantile, 0.975, na.rm = TRUE),
         n_boot = N_BOOT_IPW,
         n_boot_valid = sum(bs[, "ipw_ok"] == 1))   # 重みつきの CI に使った (重みが正常に作れた) 回数
}

message("S1 (IPW) ...")
s1_dev <- map_dfr(c("pcs", "mcs"), \(dom)
  s1_boot(D, dom, with_e4 = FALSE, strata = rep(1, nrow(D)), seed = SEED + 610) |>
    mutate(set = "D", outcome = paste0("y_", dom, "_18m")))
s1_ext <- map_dfr(c("pcs", "mcs"), \(dom)
  s1_boot(V, dom, with_e4 = TRUE, strata = V$val_fy, seed = SEED + 620) |>
    mutate(set = "V", outcome = paste0("y_", dom, "_18m")))

# 重みの分布の要約 (内部診断 §7.13 (c) 5。個人の重みは保存しない)
ipw_diag <- map_dfr(list(D = D, V = V), \(d) map_dfr(c("pcs", "mcs"), \(dom) {
  wt <- ipw_weights(d, dom, "18m")
  # 重みが 0 でない人 (台帳で確定した人 = 1 と、回答者 = 1 / 回答確率) の分布
  q <- quantile(wt$w[wt$w > 0], c(0, 0.25, 0.5, 0.75, 0.99, 1))
  tibble(outcome = paste0("y_", dom, "_18m"), n_need_sf8 = wt$n_need, n_responded = wt$n_resp,
         cap_99pct = wt$cap, n_truncated = wt$n_trunc,
         w_min = q[1], w_q1 = q[2], w_median = q[3], w_q3 = q[4], w_p99 = q[5], w_max = q[6])
}), .id = "set")

# =============================================================================
# S2: 特定の仮定による再分類 (§7.7 S2)
# =============================================================================
# 結果不明者 (SF-8 の PCS が得られない [未返送・項目の欠測・期限後の回答] か転出し、判定日までに
# 要介護 3 以上にも死亡にも至っていない人) のうち、
# 健診日より後、判定日までに初めて要介護 1 以上の認定を受けた人を、低い HRQOL であったとみなしてイベントとする。
# 結果不明者は要介護 3 以上に至っていないので、実際には「新たに要介護 1–2 になった人」にあたる
# (要介護 3 以上はすでに複合アウトカムのイベント)。ltc1_18m は 01 で作成し、健診日以前に
# 要介護 1 以上の認定歴がある人は FALSE。
# primary と同じ手順 (λ の選択を含む bootstrap) で、健診モデルの AUC と E5 の ΔAUC を求める
message("S2 ...")
d_s2 <- D |>
  mutate(y_s2 = if_else(is.na(y_pcs_18m) & ltc1_18m, 1L, y_pcs_18m)) |>
  filter(!is.na(y_s2))
s2_n_reclassified <- sum(is.na(D$y_pcs_18m) & D$ltc1_18m)
s2 <- run_dev_bootstrap(d_s2, d_s2$y_s2, c("checkup", "direct"),
                        seed_final = SEED + 100, seed_boot = SEED + 520, cache_name = "boot_s2_pcs_18m")
s2_perf  <- summarise_optimism(s2$orig, s2$metrics) |> mutate(analysis = "S2")
s2_delta <- summarise_delta(s2$orig, s2$metrics, PAIR_E5) |> mutate(analysis = "S2")
s2_health <- summarise_boot_health(s2$metrics, s2$grid) |> mutate(analysis = "S2")

# =============================================================================
# S3: 低 HRQOL の閾値を PCS < 35、< 45 に変える (§7.7 S3)
# =============================================================================
# 結果が確定する人は閾値によらず同じなので、primary と同じ乱数種 (同じ bootstrap 標本) を使う
message("S3 ...")
s3 <- map(c(35, 45), \(thr) {
  y_thr <- make_composite(D$pcs_18m, D$ltc3_18m, D$death_18m, D$moved_18m, thr)
  d <- D[!is.na(y_thr), ]; y <- y_thr[!is.na(y_thr)]
  stopifnot(identical(d$id, D$id[!is.na(D$y_pcs_18m)]))   # primary と同じ対象者
  r <- run_dev_bootstrap(d, y, c("checkup", "direct"), seed_final = SEED + 100, seed_boot = SEED + 1,
                         cache_name = paste0("boot_s3_pcs", thr, "_18m"))
  list(perf = summarise_optimism(r$orig, r$metrics) |> mutate(analysis = paste0("S3_", thr)),
       delta = summarise_delta(r$orig, r$metrics, PAIR_E5) |> mutate(analysis = paste0("S3_", thr)),
       health = summarise_boot_health(r$metrics, r$grid) |> mutate(analysis = paste0("S3_", thr)),
       events = sum(y), n = length(y))
}) |> set_names(c("S3_35", "S3_45"))

# =============================================================================
# S5: 罰則なしのロジスティック回帰 + 一様縮小 (§7.7 S5)
# =============================================================================
# 1. 罰則なしのロジスティック回帰を開発データで推定する
# 2. bootstrap (primary と同じ標本) で、縮小前のモデルの推定と性能評価を繰り返し、
#    optimism 補正 AUC と、元のデータでの calibration slope の平均 (= 縮小係数) を求める
# 3. 全係数に縮小係数を掛け、切片を開発データで推定し直す [PMID 2277880]
# 4. 縮小後のモデルを外部検証 (V) に当てはめて較正を評価する
# AUC は係数を一様に縮小しても変わらない (順位が変わらない) ので、縮小前のモデルで評価する
message("S5 ...")
S5_MODELS <- c("checkup", "direct", "questionnaire")
d5 <- D |> filter(!is.na(y_pcs_18m)); y5 <- d5$y_pcs_18m
X5 <- map(MODELS[S5_MODELS], \(v) make_x(d5, v))
# 罰則なしのロジスティック回帰。
# bootstrap 標本では、少ないカテゴリの人が 1 人もいないと、その列の係数が推定できず NA になる。
# NA のままだと予測値がすべて NA になるので、その係数は 0 とする (その標本ではそのカテゴリを
# 参照カテゴリと同じとみなす) し、件数 (n_aliased) を記録する。
# 収束しなかった回や係数が有限でない回は ok = FALSE とし、bootstrap では失敗として除く
fit_glm <- function(x, y) {
  f <- suppressWarnings(glm.fit(cbind(1, x), y, family = binomial()))
  b <- f$coefficients
  n_aliased <- sum(is.na(b))
  b[is.na(b)] <- 0
  list(coef = b, converged = isTRUE(f$converged), n_aliased = n_aliased,
       ok = isTRUE(f$converged) && all(is.finite(b)))
}
orig5 <- imap_dfr(X5, \(x, m) {
  f <- fit_glm(x, y5)
  # 元のデータでの推定は正常に終わっていなければならない
  if (!f$ok || f$n_aliased > 0) stop("S5: 元のデータで罰則なしのモデルを推定できませんでした (", m, ")")
  as_tibble_row(perf_stats(y5, predict_lp(f$coef, x))) |> mutate(model = m)
})
design5 <- make_boot_design(nrow(d5), N_BOOT, seed = SEED + 1)   # primary と同じ bootstrap 標本
boot5 <- map_dfr(seq_len(N_BOOT), \(b) {
  idx <- design5$idx[, b]
  imap_dfr(X5, \(x, m) {
    f <- fit_glm(x[idx, , drop = FALSE], y5[idx])
    if (f$ok) {
      app  <- perf_stats(y5[idx], predict_lp(f$coef, x[idx, , drop = FALSE]))
      test <- perf_stats(y5, predict_lp(f$coef, x))
    } else {
      app <- test <- c(auc = NA_real_, slope = NA_real_, citl = NA_real_)   # 失敗した回 (集計から除く)
    }
    tibble(b = b, model = m, converged = f$converged, n_aliased = f$n_aliased, ok = f$ok,
           auc_app = app[["auc"]], auc_test = test[["auc"]],
           slope_app = app[["slope"]], slope_test = test[["slope"]],
           citl_app = app[["citl"]], citl_test = test[["citl"]])
  })
})
s5_perf  <- summarise_optimism(orig5, boot5) |> mutate(analysis = "S5")
s5_delta <- summarise_delta(orig5, boot5,
                            tibble(contrast = c("E5", "E8"), model1 = c("direct", "checkup"),
                                   model2 = c("checkup", "questionnaire"))) |>
  mutate(analysis = "S5")
# 縮小係数と、縮小後のモデルの外部検証での較正
s5_ext <- imap_dfr(X5[c("checkup", "direct")], \(x, m) {
  f <- fit_glm(x, y5)
  shrink <- mean(boot5$slope_test[boot5$model == m & boot5$ok], na.rm = TRUE)
  lp_shrunk_nointercept <- as.numeric(x %*% (shrink * f$coef[-1]))
  b0 <- coef(glm(y5 ~ 1, offset = lp_shrunk_nointercept, family = binomial()))[[1]]
  coef_s <- c(b0, shrink * f$coef[-1])
  dv <- V |> filter(!is.na(y_pcs_18m))
  ext_perf(dv$y_pcs_18m, predict_lp(coef_s, make_x(dv, MODELS[[m]]))) |>
    mutate(model = m, shrinkage = shrink)
})
# bootstrap の健全性: 収束しなかった回・失敗した回 (除外)・係数を 0 にした回の件数 (07 で確認する)
s5_nonconv <- boot5 |>
  group_by(model) |>
  summarise(n_boot = n(), n_not_converged = sum(!converged), n_failed = sum(!ok),
            n_with_aliased = sum(n_aliased > 0), .groups = "drop")

# =============================================================================
# eFigure 4 のデータ: 健診モデル (約 1 年半後、PCS 複合) の AUC と E5 の ΔAUC を並べる
# =============================================================================
# 性能の種類 (optimism 補正 / 見かけ / 外部検証) が違う値を並べるので、type 列で区別する
pd <- perf_dev$perf |> filter(outcome == "y_pcs_18m", model == "checkup", stat == "auc")
dd <- perf_dev$delta |> filter(outcome == "y_pcs_18m", contrast == "E5")
s1p <- s1_dev |> filter(outcome == "y_pcs_18m")
g <- \(q, col) s1p[[col]][s1p$quantity == q]
row_corr <- function(analysis, label, perf, delta) {
  p <- perf |> filter(model == "checkup", stat == "auc"); dl <- delta |> filter(contrast == "E5")
  tibble(analysis = analysis, label = label, type = "Optimism-corrected",
         auc = p$corrected, auc_lo = p$lo, auc_hi = p$hi,
         d_e5 = dl$corrected, d_e5_lo = dl$lo, d_e5_hi = dl$hi)
}
row_s4 <- function(set, label) {
  p <- perf_ext$perf |> filter(set == !!set, outcome == "y_pcs_18m", model == "checkup", stat == "auc")
  dl <- perf_ext$delta |> filter(set == !!set, outcome == "y_pcs_18m", contrast == "E5")
  # S4 は開発の参加者の後年の予測を含むので、時間的外部検証 (V) とは区別して「後年の性能評価」と表示する (§7.0)
  tibble(analysis = "S4", label = label, type = "Later-year performance",
         auc = p$estimate, auc_lo = p$lo, auc_hi = p$hi,
         d_e5 = dl$estimate, d_e5_lo = dl$lo, d_e5_hi = dl$hi)
}
efig3 <- bind_rows(
  tibble(analysis = "Primary", label = "Primary analysis", type = "Optimism-corrected",
         auc = pd$corrected, auc_lo = pd$lo, auc_hi = pd$hi,
         d_e5 = dd$corrected, d_e5_lo = dd$lo, d_e5_hi = dd$hi),
  tibble(analysis = "Unweighted", label = "Final model, unweighted", type = "Apparent",
         auc = g("unw.auc", "estimate"), auc_lo = g("unw.auc", "lo"), auc_hi = g("unw.auc", "hi"),
         d_e5 = g("unw_d_e5", "estimate"), d_e5_lo = g("unw_d_e5", "lo"), d_e5_hi = g("unw_d_e5", "hi")),
  tibble(analysis = "S1", label = "S1: inverse probability weighting", type = "Apparent",
         auc = g("ipw.auc", "estimate"), auc_lo = g("ipw.auc", "lo"), auc_hi = g("ipw.auc", "hi"),
         d_e5 = g("ipw_d_e5", "estimate"), d_e5_lo = g("ipw_d_e5", "lo"), d_e5_hi = g("ipw_d_e5", "hi")),
  row_corr("S2", "S2: reclassification of unknown outcomes", s2_perf, s2_delta),
  row_corr("S3_35", "S3: PCS < 35", s3$S3_35$perf, s3$S3_35$delta),
  row_corr("S3_45", "S3: PCS < 45", s3$S3_45$perf, s3$S3_45$delta),
  row_s4("S4_FY2022", "S4: all FY2022 participants"),
  row_s4("S4_FY2023", "S4: all FY2023 participants"),
  row_corr("S5", "S5: unpenalized + uniform shrinkage", s5_perf, s5_delta)
)
print(efig3, width = Inf)

save_derived(list(
  efig3 = efig3, s1_dev = s1_dev, s1_ext = s1_ext, ipw_diag = ipw_diag,
  s2 = list(perf = s2_perf, delta = s2_delta, n_reclassified = s2_n_reclassified,
            n = nrow(d_s2), events = sum(d_s2$y_s2)),
  s3 = map(s3, \(x) x[c("perf", "delta", "n", "events")]),
  # S2・S3 の bootstrap の健全性 (07 の内部診断で確認する)
  boot_health = bind_rows(s2_health, map_dfr(s3, "health")),
  s5 = list(perf = s5_perf, delta = s5_delta, ext = s5_ext, nonconverged = s5_nonconv)
), "sens")
message("05 完了")
