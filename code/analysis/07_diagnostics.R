# =============================================================================
# 07_diagnostics.R  解析後の内部診断 (非公表)
# -----------------------------------------------------------------------------
# 目的 : 推定がうまくいっているかを確かめる (§7.13 (c) 4・5)。
#   4. bootstrap の optimism、選ばれた λ、calibration slope の分布と、
#      推定できなかった標本 (指標が NA) や λ が候補列の端に張りついた標本の有無
#   5. IPW の重みの分布 (極端な重み、切り詰めの影響)
#   (データに関する点検 1–3・6・7 は 01b で行う)
# 入力 : output/derived/boot_dist.rds (03), sens.rds (05), perf_dev.rds (03)
# 出力 : output/derived/diag_boot.rds  bootstrap の分布の要約 (数値)
#        output/derived/diag_ipw.rds   IPW の重みの要約
#        図は 99_RENDER_outputs.R が output/checks/ に出す
# 時間 : 数秒
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
boot_dist <- read_derived("boot_dist")
sens      <- read_derived("sens")
perf_dev  <- read_derived("perf_dev")

# ---- 1. bootstrap の分布の要約 ----
# optimism = 自分の標本での性能 − 元のデータでの性能。
# slope_test = bootstrap 標本で推定したモデルを元のデータに当てはめたときの calibration slope
#   (1 より小さいほど過学習。その平均が一様縮小の目安になる)
q <- function(x) {
  x <- x[!is.na(x)]
  tibble(mean = mean(x), p2.5 = quantile(x, 0.025), median = median(x), p97.5 = quantile(x, 0.975))
}
diag_boot <- boot_dist |>
  mutate(auc_optimism = auc_app - auc_test) |>
  group_by(outcome, model) |>
  summarise(n_boot = n(),
            n_success = sum(!is.na(auc_test) & !is.na(slope_test)),
            n_failed = n_boot - n_success,   # 指標を計算できなかった標本 (割合の分母は試行数 n_boot)
            # λ の列の途中で glmnet の推定が止まった標本 (CV の中 / 選んだ λ での推定)
            # (fit_truncated = 選んだ λ で推定し直した回。推定し直しても失敗した回は n_failed に数える)
            n_cv_truncated = sum(cv_truncated > 0, na.rm = TRUE), n_fit_truncated = sum(fit_truncated, na.rm = TRUE),
            # 失敗した標本は λ が NA なので除いて数える
            n_lambda_at_max = sum(lambda_at_max, na.rm = TRUE),   # 罰則が最も強い端 (ほぼ切片だけ)
            n_lambda_at_min = sum(lambda_at_min, na.rm = TRUE),   # 罰則が最も弱い端 (罰則なしに近い)
            # 出力の列名は元の列名と変える (summarise の中で同じ名前を使うと、後の行で上書き後の値が使われる)
            log_lambda = list(q(log(lambda))), lambda_dist = list(q(lambda)),
            auc_optimism_dist = list(q(auc_optimism)), slope_test_dist = list(q(slope_test)),
            .groups = "drop") |>
  unnest(c(log_lambda, lambda_dist, auc_optimism_dist, slope_test_dist), names_sep = "_") |>
  rename_with(\(x) str_remove(x, "_dist"))

# 最終モデルで選ばれた λ が候補列の端でないか
final <- read_derived("final_models")
diag_final_lambda <- imap_dfr(final, \(fm, yv) imap_dfr(fm, \(f, m)
  tibble(outcome = yv, model = m, lambda = f$lambda, lambda_index = f$lambda_index,
         cv_truncated = f$cv_truncated, fit_truncated = f$fit_truncated,
         grid_min = f$lambda_grid_range[1], grid_max = f$lambda_grid_range[2],
         at_max = f$lambda == grid_max, at_min = f$lambda == grid_min)))

# ---- 2. 警告として表示する条件 ----
warn <- c(
  if (isTRUE(any(diag_boot$n_failed > 0))) "指標を計算できなかった bootstrap 標本があります",
  if (isTRUE(any(diag_boot$n_cv_truncated > 0 | diag_boot$n_fit_truncated > 0)))
    "λ の列の途中で glmnet の推定が止まった bootstrap 標本があります (diag_boot を確認)",
  if (isTRUE(any(diag_final_lambda$cv_truncated > 0 | diag_final_lambda$fit_truncated)))
    "最終モデルの推定で、λ の列の途中で glmnet の推定が止まりました",
  if (isTRUE(any(diag_boot$n_lambda_at_max > 0.05 * diag_boot$n_boot)))
    "5% を超える bootstrap 標本で、λ が候補列の最大の端 (ほぼ切片だけのモデル) に選ばれています",
  # 最小の端に張りつくと、CV の逸脱度が最小となる λ が候補の範囲で打ち切られている (§7.5)。
  # 本番の実行後に 0 件であることを確かめ、あれば R/fn_model.R の候補列を小さい側に広げて計算し直す
  if (isTRUE(any(diag_boot$n_lambda_at_min > 0)))
    "λ が候補列の最小の端に選ばれた bootstrap 標本があります (候補列を小さい側に広げて計算し直す)",
  if (isTRUE(any(diag_final_lambda$at_max | diag_final_lambda$at_min))) "最終モデルの λ が候補列の端です",
  if (isTRUE(any(sens$s5$nonconverged$n_failed > 0)))
    "S5 で推定に失敗した (収束しなかった) bootstrap 標本があります (集計から除外)",
  if (isTRUE(any(sens$s5$nonconverged$n_with_aliased > 0)))
    "S5 でカテゴリの欠けた bootstrap 標本があります (その係数を 0 として評価)",
  # S1 (IPW): 回答確率モデルの推定が正常でなかった bootstrap 標本 (重みつきの CI から除外)
  if (isTRUE(any(c(sens$s1_dev$n_boot_valid, sens$s1_ext$n_boot_valid) < N_BOOT_IPW)))
    "S1 で回答確率モデルの推定が正常でなかった bootstrap 標本があります (除外)"
)
# 感度分析 S2・S3 の bootstrap の健全性 (05 で集計)
sens_health <- sens$boot_health
warn <- c(warn,
  if (isTRUE(any(sens_health$n_failed > 0))) "S2・S3 で指標を計算できなかった bootstrap 標本があります",
  if (isTRUE(any(sens_health$n_cv_truncated > 0 | sens_health$n_fit_truncated > 0)))
    "S2・S3 で λ の列の途中で glmnet の推定が止まった bootstrap 標本があります",
  if (isTRUE(any(sens_health$n_lambda_at_min > 0 | sens_health$n_lambda_at_max > 0)))
    "S2・S3 で λ が候補列の端に選ばれた bootstrap 標本があります")
if (length(warn)) message("注意: ", paste(warn, collapse = " / ")) else message("内部診断: 問題なし")
if (!is.null(sens_health)) print(as.data.frame(sens_health))
print(as.data.frame(sens$s5$nonconverged))
message("S1 の有効な bootstrap 回数: ", paste(unique(c(sens$s1_dev$n_boot_valid, sens$s1_ext$n_boot_valid)), collapse = ", "),
        " / ", N_BOOT_IPW)

print(diag_boot |> select(outcome, model, n_failed, n_cv_truncated, n_fit_truncated, n_lambda_at_max, n_lambda_at_min,
                          auc_optimism_mean, slope_test_mean, slope_test_p2.5, slope_test_p97.5),
      n = Inf, width = Inf)
print(sens$ipw_diag, width = Inf)

save_derived(list(boot = diag_boot, final_lambda = diag_final_lambda, sens = sens_health, warnings = warn),
             "diag_boot")
save_derived(sens$ipw_diag, "diag_ipw")
message("07 完了")
