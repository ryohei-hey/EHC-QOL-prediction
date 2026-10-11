# =============================================================================
# 06_missing_comparison.R  結果が確定した人と結果不明の人の比較 (eTable 2)
# -----------------------------------------------------------------------------
# 目的 : 約 1 年半後の primary アウトカム (PCS 複合) で、結果が確定した人と結果不明の人の
#        健診時の特性を比べ、complete case による選択バイアスの方向を検討する材料にする (§7.8)。
#        開発 (D) と外部検証 (V) のそれぞれで比べる。
# 入力 : data/processed/analysis_sets.rds (01)
# 出力 : output/derived/etbl4.rds  (gtsummary の表の中身。集計済みの文字列だけで個人単位のデータは含まない)
# 対応 : protocol §6.3 (b)・§7.8・§7.13 (b) eTable 2
# 時間 : 数秒
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(gtsummary))
as_sets <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))

# 比べる変数: 年齢・性別・質問票 15 項目・健診項目 (元の単位)・健診前の PCS/MCS (§7.8)
ETBL4_VARS <- c("age", "sex", ITEMS,
                "bmi", "sbp", "dbp", "hdl", "ldl", "raw_tg", "fasting",
                "raw_ast", "raw_alt", "raw_ggtp", "hba1c", "egfr", "upro", "uglu",
                "pcs_pre", "mcs_pre")

make_etbl4 <- function(d, set) {
  dd <- d |>
    mutate(status = factor(if_else(is.na(y_pcs_18m), "Unknown", "Known"),
                           levels = c("Known", "Unknown"))) |>
    select(status, all_of(ETBL4_VARS))
  tb <- tbl_summary(dd, by = status,
                    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                                     all_categorical() ~ "{n} ({p}%)"),
                    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
                    missing = "no")
  list(body = tb$table_body |> select(variable, var_type, row_type, label, stat_1, stat_2),
       n = c(Known = sum(dd$status == "Known"), Unknown = sum(dd$status == "Unknown")),
       set = set)
}

etbl4 <- list(D = make_etbl4(as_sets$D, "Development"),
              V = make_etbl4(as_sets$V, "Validation"))
save_derived(etbl4, "etbl4")
message("06 完了")
