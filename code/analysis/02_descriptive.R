# =============================================================================
# 02_descriptive.R  記述統計・対象者のフロー・時点の間隔・予測因子の定義
# -----------------------------------------------------------------------------
# 目的 : Table 1・eTable 1・eFigure 1・eFigure 2 のもとになるデータを作って保存する。
#        図や表の見た目 (ラベル・書式) は 99_RENDER_outputs.R で決める。
# 入力 : data/processed/analysis_sets.rds (01)
# 出力 : output/derived/tbl1.rds           Table 1 (全体・開発・検証の要約。集計済みの文字列)
#        output/derived/etbl1_predictors.rds eTable 1 (予測因子の定義)
#        output/derived/efig1_timeline.rds  eFigure 1 (健診と調査の時期、健診からの間隔)
#        output/derived/efig2_flow.rds      eFigure 2 (対象者のフロー)
#        output/derived/efig2_flow_counts.csv eFigure 2 の人数 (原稿の数値との照合用)
# 対応 : protocol §6.3 (a)・§7.4・§7.13 (Table 1, eTable 1, eFigure 1, eFigure 2)
# 時間 : 数秒
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(gtsummary))
as_sets <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))

# ---- 1. Table 1: 背景特性 (全体・開発 D・検証 V の 3 列) ----
# 連続変数は median (IQR)、カテゴリは n (%) (§7.4)。
# 検査値は対数変換の前の値 (元の単位) で示す。変数名は 99 で英語の表示名に置き換える。
TBL1_VARS <- c("age", "sex", ITEMS,
               "bmi", "sbp", "dbp", "hdl", "ldl", "raw_tg", "fasting",
               "raw_ast", "raw_alt", "raw_ggtp", "hba1c", "egfr", "upro", "uglu",
               "pcs_pre", "mcs_pre")
tbl1_data <- bind_rows(
  as_sets$D |> mutate(cohort = "Development"),
  as_sets$V |> mutate(cohort = "Validation")
) |>
  select(cohort, all_of(TBL1_VARS)) |>
  mutate(cohort = factor(cohort, levels = c("Development", "Validation")))

tbl1 <- tbl_summary(
  tbl1_data, by = cohort,
  statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                   all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
  missing = "no"
) |>
  add_overall()

# gtsummary の表の中身 (table_body) だけを保存する。
# 集計済みの文字列だけで個人単位のデータは含まないので、output/derived に置ける。
# 列: variable (変数名), row_type (label = 変数の行 / level = カテゴリの行), label, stat_0 (全体),
#     stat_1 (開発), stat_2 (検証)
tbl1_body <- tbl1$table_body |>
  select(variable, var_type, row_type, label, stat_0, stat_1, stat_2)
tbl1_n <- c(Overall = nrow(tbl1_data), Development = nrow(as_sets$D), Validation = nrow(as_sets$V))
save_derived(list(body = tbl1_body, n = tbl1_n), "tbl1")

# ---- 2. eTable 1: 予測因子の定義 ----
# プロトコル §7.13 (b') のドラフト (英語) をそのままデータにしたもの。
# QMCOO の設問は公式の英語版 (Satake & Arai 2020, PMID 33003256) による
etbl1 <- tribble(
  ~domain, ~predictor, ~wording, ~response, ~coding, ~source,
  "Demographics", "Age", "Age at the health checkup", "Years", "Continuous, linear", "Checkup record",
  "Demographics", "Sex", "—", "Female (Ref) / Male", "Binary", "Checkup record",
  "Health condition", "Self-rated health (Q1)", "How is your health condition?", "Excellent (Ref) / Good / Fair / Poor / Very poor", "Categorical (4 dummies)", "QMCOO",
  "Mental health", "Life satisfaction (Q2)", "Are you satisfied with your daily life?", "Satisfied (Ref) / Moderately satisfied / Moderately dissatisfied / Dissatisfied", "Categorical (3 dummies)", "QMCOO",
  "Eating behavior", "Regular meals (Q3)", "Do you eat three times a day?", "Yes (Ref) / No", "Binary", "QMCOO",
  "Oral function", "Chewing difficulty (Q4)", "Do you have any difficulties eating tough foods compared to 6 months ago?", "No (Ref) / Yes", "Binary", "QMCOO",
  "Oral function", "Choking (Q5)", "Have you choked on your tea or soup recently?", "No (Ref) / Yes", "Binary", "QMCOO",
  "Body weight loss", "Weight loss (Q6)", "Have you lost 2 kg or more in the past 6 months?", "No (Ref) / Yes", "Binary", "QMCOO",
  "Physical function and falls", "Slow walking (Q7)", "Do you think you walk slower than before?", "No (Ref) / Yes", "Binary", "QMCOO",
  "Physical function and falls", "Falls (Q8)", "Have you experienced a fall in the past year?", "No (Ref) / Yes", "Binary", "QMCOO",
  "Physical function and falls", "Walking for exercise (Q9)", "Do you go for a walk for your health at least once a week?", "Yes (Ref) / No", "Binary", "QMCOO",
  "Cognitive function", "Memory complaints (Q10)", "Do your family or friends point out your memory loss? (e.g., \"You ask the same question over and over again.\")", "No (Ref) / Yes", "Binary", "QMCOO",
  "Cognitive function", "Disorientation to date (Q11)", "Do you find yourself not knowing today's date?", "No (Ref) / Yes", "Binary", "QMCOO",
  "Smoking", "Smoking (Q12)", "Do you smoke?", "No (Ref) / I quit / Yes", "Categorical (2 dummies)", "QMCOO",
  "Social participation", "Going out (Q13)", "Do you go out at least once a week?", "Yes (Ref) / No", "Binary", "QMCOO",
  "Social participation", "Social contact (Q14)", "Do you keep regular communication with your family and friends?", "Yes (Ref) / No", "Binary", "QMCOO",
  "Social support", "Social support (Q15)", "When you are not feeling well, do you have anyone you can talk with?", "Yes (Ref) / No", "Binary", "QMCOO",
  "Anthropometry", "Body mass index", "Measured height and weight", "kg/m²", "Continuous, linear", "Checkup record",
  "Blood pressure", "Systolic blood pressure", "Measured at the checkup", "mmHg", "Continuous, linear", "Checkup record",
  "Blood pressure", "Diastolic blood pressure", "Measured at the checkup", "mmHg", "Continuous, linear", "Checkup record",
  "Blood tests", "HDL cholesterol", "Serum", "mg/dL", "Continuous, linear", "Checkup record",
  "Blood tests", "LDL cholesterol", "Serum", "mg/dL", "Continuous, linear", "Checkup record",
  "Blood tests", "Triglycerides", "Serum", "mg/dL", "Natural log, linear", "Checkup record",
  "Blood tests", "Time since last meal at blood sampling", "Recorded at blood sampling", "≥10 h (Ref) / 3.5 to <10 h / <3.5 h", "Categorical (2 dummies)", "Checkup record",
  "Blood tests", "AST", "Serum", "U/L", "Natural log, linear", "Checkup record",
  "Blood tests", "ALT", "Serum", "U/L", "Natural log, linear", "Checkup record",
  "Blood tests", "γ-GTP", "Serum", "U/L", "Natural log, linear", "Checkup record",
  "Blood tests", "HbA1c (NGSP)", "Blood", "%", "Continuous, linear", "Checkup record",
  "Blood tests", "eGFR", "Calculated from serum creatinine with the Japanese equation: 194 × Cr^−1.094 × age^−0.287 (× 0.739 if female)", "mL/min/1.73 m²", "Continuous, linear", "Checkup record",
  "Urinalysis", "Urine protein", "Dipstick", "− (Ref) / ± / ≥1+", "Categorical (2 dummies)", "Checkup record",
  "Urinalysis", "Urine glucose", "Dipstick", "− (Ref) / ≥±", "Binary", "Checkup record"
)
# 定義表のダミー変数の数の合計が、モデルのパラメータ数 (39) と一致することを確かめる
n_param <- sum(case_when(
  str_detect(etbl1$coding, "\\((\\d+) dummies\\)") ~ as.numeric(str_extract(etbl1$coding, "\\d+(?= dummies)")),
  .default = 1))
stopifnot(n_param == ncol(make_x(as_sets$D, MODELS$checkup)))
save_derived(etbl1, "etbl1_predictors")

# ---- 3. eFigure 1: 健診と調査の時期、健診からの間隔 ----
# コホートごとに、健診の実施期間と、健診前・約半年後・約 1 年半後の調査までの間隔
# (最小・中央値・最大、年) を実データから求める (§6.2)。間隔は判定日 (3 月 31 日) までの年数
interval_summary <- function(x) tibble(min = min(x), median = median(x), max = max(x))
pre_years <- function(d, yr) as.numeric(d$exam_date - as.Date(paste0(yr, "-03-31"))) / 365.25
V22 <- filter(as_sets$V, val_fy == "FY2022"); V23 <- filter(as_sets$V, val_fy == "FY2023")
timeline <- bind_rows(
  tibble(cohort = "Development (FY2020)", n = nrow(as_sets$D),
         exam_start = min(as_sets$D$exam_date), exam_end = max(as_sets$D$exam_date),
         survey = c("pre", "6m", "18m"), survey_date = as.Date(c("2020-03-31", "2021-03-31", "2022-03-31")),
         bind_rows(interval_summary(pre_years(as_sets$D, 2020)),
                   interval_summary(as_sets$D$t_6m), interval_summary(as_sets$D$t_18m))),
  tibble(cohort = "Validation (FY2022)", n = nrow(V22),
         exam_start = min(V22$exam_date), exam_end = max(V22$exam_date),
         survey = c("pre", "18m"), survey_date = as.Date(c("2022-03-31", "2024-03-31")),
         bind_rows(interval_summary(pre_years(V22, 2022)), interval_summary(V22$t_18m))),
  tibble(cohort = "Validation (FY2023)", n = nrow(V23),
         exam_start = min(V23$exam_date), exam_end = max(V23$exam_date),
         survey = c("pre", "18m"), survey_date = as.Date(c("2022-03-31", "2025-03-31")),
         bind_rows(interval_summary(pre_years(V23, 2022)), interval_summary(V23$t_18m))),
  tibble(cohort = "Validation, approximately 6 months (FY2023)", n = nrow(as_sets$V6m),
         exam_start = min(as_sets$V6m$exam_date), exam_end = max(as_sets$V6m$exam_date),
         survey = c("pre", "6m"), survey_date = as.Date(c("2022-03-31", "2024-03-31")),
         bind_rows(interval_summary(pre_years(as_sets$V6m, 2022)), interval_summary(as_sets$V6m$t_6m)))
)
# SF-8 のない 2023-03 の調査 (QGEN10) も図に示すため、別に持っておく
timeline_no_sf8 <- tibble(survey_date = as.Date("2023-03-31"), label = "No SF-8 (QGEN10)")
save_derived(list(timeline = timeline, no_sf8 = timeline_no_sf8), "efig1_timeline")

# ---- 4. eFigure 2: 対象者のフロー ----
# 年度ごとの組入・除外の段階と、時点ごとの結果確定 / 結果不明 / イベントの内訳 (§5・§7.1)
flow_steps <- imap_dfr(as_sets$flow[c("FY2020", "FY2022", "FY2023")], \(f, fy)
  tibble(fiscal_year = fy, step = names(f), n = unname(f), order = seq_along(f)))
outcome_flow <- function(d, set, tag) {
  map_dfr(c("pcs", "mcs"), \(dom) {
    y <- d[[paste0("y_", dom, "_", tag)]]; type <- d[[paste0("type_", dom, "_", tag)]]
    tibble(set = set, time = tag, domain = dom, n_eligible = nrow(d),
           n_known = sum(!is.na(y)), n_unknown = sum(is.na(y)),
           n_unknown_moved = sum(d[[paste0("moved_", tag)]]),
           events = sum(y %in% 1), low = sum(type %in% "low"),
           ltc3 = sum(type %in% "ltc3"), death = sum(type %in% "death"))
  })
}
flow_outcomes <- bind_rows(
  outcome_flow(as_sets$D, "Development", "6m"),
  outcome_flow(as_sets$D, "Development", "18m"),
  outcome_flow(V22, "Validation FY2022", "18m"),
  outcome_flow(V23, "Validation FY2023", "18m"),
  outcome_flow(as_sets$V, "Validation", "18m"),
  outcome_flow(as_sets$V6m, "Validation 6 months", "6m")
)
flow_assign <- tibble(set = c("Validation FY2022", "Validation FY2023", "Validation 6 months"),
                      n = c(as_sets$flow$V[["FY2022"]], as_sets$flow$V[["FY2023"]], as_sets$flow$V6m))
# フロー図: 時間の順に、健診前の 3 月調査の対象住民 → 回答なし → 3 月調査の回答者 → 組入基準 (§5.1) を満たさない
# → 組入基準を満たす → 除外 (§5.2) → 解析コホート。
# 調査の対象住民・発送・返送の人数は市の調査対象選定資料の集計値、それより下は個人データ
# (01 の screen_survey = 3 月調査の回答者のうち研究への同意者) から flow_from_data() (R/flow_helpers.R) で数える。
# 返送数と研究データの人数の差は、研究への同意がない人とする。
# 1 人が複数の理由に当たるときは、reasons に書いた順で最初の理由に数える
sel_path <- list.files("reference", pattern = "選定過程資料.*[.]xlsx$", full.names = TRUE)
stopifnot(length(sel_path) == 1)
sel <- readxl::read_excel(sel_path, col_names = FALSE, .name_repair = "minimal")
sel_row <- \(prefix) as.character(unlist(sel[str_starts(sel[[1]], prefix) %in% TRUE, ][1, ]))
# 3 月に発送した調査の列 (追加調査 [10 月] の列は使わない)
survey_counts <- function(yr) {
  col <- which(sel_row("調査票作成・発送年度") == as.character(yr) & str_ends(sel_row("調査票発送時期"), "3月"))
  stopifnot(length(col) == 1)
  c(target = as.integer(sel_row("➀対象年齢")[col]), sent = as.integer(sel_row("実発送数")[col]),
    returned = as.integer(sel_row("調査票回答")[col]))
}
not_eligible <- function(fy) list(label = "Did not meet inclusion criteria",
                                  reasons = setNames(c("flag_sf8", "flag_nochk"),
                                                     c("SF-8 not scored", sprintf("Did not attend the %s checkup", fy))))
excl_base <- c("Questionnaire items missing" = "flag_items",
               "Checkup measurements missing" = "flag_chk",
               "Care level ≥3 at checkup" = "flag_ltc3")
# 個人データで数えたフローの上に、集計値の段 (調査の対象住民 → 発送されない・返送しない・同意がない) を足す
add_survey_stage <- function(f, yr, n_consented) {
  sc <- survey_counts(yr)
  no_resp <- c("Not sent the survey" = sc[["target"]] - sc[["sent"]],
               "Did not return the survey" = sc[["sent"]] - sc[["returned"]])
  no_consent <- c("Did not consent to research use" = sc[["returned"]] - n_consented)
  stopifnot(all(c(no_resp, no_consent) >= 0))
  # 対象住民 → (回答なし) → 3 月調査の回答者 → (組入基準を満たさない: 同意なし + 個人データの理由) → …
  # 市の資料の対象年齢「年度到達年齢 76 歳以上」は、調査の翌年度の末までに 76 歳 = 調査年の 3 月 31 日までに 75 歳。
  # 研究データの回答者の生年月日もこの範囲に収まる (最も遅い生年月日: 2020 年調査 1945-03-30、2022 年調査 1947-03-31)
  f$boxes <- c(list(list(label = sprintf("March %d survey target residents\n(aged ≥75 years by 31 March %d)", yr, yr),
                         n = sc[["target"]]),
                    list(label = sprintf("March %d survey respondents", yr), n = sc[["returned"]])),
               f$boxes[-1])
  f$exclusions[[1]]$reasons <- c(no_consent, f$exclusions[[1]]$reasons)
  f$exclusions[[1]]$n <- sum(f$exclusions[[1]]$reasons)
  f$exclusions <- c(list(list(label = "Did not respond", reasons = no_resp, n = sum(no_resp))), f$exclusions)
  check_flow(f)
}
flow_survey <- function(fy, yr, labels, steps) {
  d <- as_sets$screen_survey[[fy]]
  f <- flow_from_data(d, labels = c("(survey respondents)", labels), steps = c(list(not_eligible(fy)), steps))
  inc <- attr(f, "included")
  f <- add_survey_stage(f, yr, nrow(d))
  attr(f, "included") <- d$id[inc]
  f
}
flow_diagrams <- list(
  FY2020 = flow_survey("FY2020", 2020, c("Met inclusion criteria", "Development cohort"),
                       list(list(label = "Excluded", reasons = excl_base))),
  FY2022 = flow_survey("FY2022", 2022, c("Met inclusion criteria", "FY2022 validation cohort"),
                       list(list(label = "Excluded", reasons = c(excl_base, "In development cohort" = "flag_in_dev")))),
  FY2023 = flow_survey("FY2023", 2022, c("Met inclusion criteria", "≈6-month validation cohort", "FY2023 validation cohort"),
                       list(list(label = "Excluded", reasons = c(excl_base, "In development cohort" = "flag_in_dev")),
                            list(label = "Excluded", reasons = c("In FY2022 validation cohort" = "flag_in_v22"))))
)
# 解析コホートの箱に、時点ごとの結果不明 (Outcome unknown) の人数を書き足す。
# 結果確定の人数 (ROC 解析の対象) = 箱の人数 − 結果不明。PCS と MCS の複合で結果不明は同じ人 (確かめる)
oc_missing <- flow_outcomes |>
  filter(set %in% c("Development", "Validation FY2022", "Validation FY2023", "Validation 6 months")) |>
  group_by(set, time) |>
  summarise(n_eligible = first(n_eligible), n_unknown = first(n_unknown), same = n_distinct(n_unknown) == 1, .groups = "drop")
stopifnot(all(oc_missing$same))
missing_note <- function(set_) {
  m <- oc_missing |> filter(set == set_) |> arrange(match(time, c("6m", "18m")))
  paste(sprintf("Outcome unknown at ≈%s months (%s)", recode(m$time, "6m" = "6", "18m" = "18"),
                fmt_n(m$n_unknown)), collapse = "\n")
}
add_note <- function(f, i, set_) {
  stopifnot(all(oc_missing$n_eligible[oc_missing$set == set_] == f$boxes[[i]]$n))
  f$boxes[[i]]$note <- missing_note(set_)
  f
}
flow_diagrams$FY2020 <- add_note(flow_diagrams$FY2020, 4, "Development")
flow_diagrams$FY2022 <- add_note(flow_diagrams$FY2022, 4, "Validation FY2022")
flow_diagrams$FY2023 <- flow_diagrams$FY2023 |> add_note(4, "Validation 6 months") |> add_note(5, "Validation FY2023")
# 解析コホートがフロー図と同じ人になっているか確認する
stopifnot(
  setequal(attr(flow_diagrams$FY2020, "included"), as_sets$D$id),
  setequal(attr(flow_diagrams$FY2022, "included"), as_sets$ids$V22),
  setequal(attr(flow_diagrams$FY2023, "included"), as_sets$ids$V23),
  flow_diagrams$FY2023$boxes[[4]]$n == nrow(as_sets$V6m)
)
save_derived(list(steps = flow_steps, assign = flow_assign, outcomes = flow_outcomes, diagrams = flow_diagrams),
             "efig2_flow")
# 図の人数を 1 行 1 項目で書き出す (原稿の数値との照合用)
write.csv(bind_rows(
            imap_dfr(flow_diagrams, \(f, fy) tibble(fiscal_year = fy, flow_counts_table(f))),
            # 解析コホートごとの結果不明と結果確定 (解析対象) の人数
            oc_missing |>
              transmute(fiscal_year = recode(set, "Development" = "FY2020", "Validation FY2022" = "FY2022",
                                             "Validation FY2023" = "FY2023", "Validation 6 months" = "FY2023"),
                        set, time, missing = n_unknown, analysed = n_eligible - n_unknown) |>
              pivot_longer(c(missing, analysed), names_to = "level", values_to = "n") |>
              mutate(label = sprintf("%s, %s outcome", set, time), .keep = "unused")
          ) |>
            mutate(label = str_replace_all(label, "\n", " ")),
          file.path(DIR_DERIVED, "efig2_flow_counts.csv"), row.names = FALSE, fileEncoding = "UTF-8")

message("02 完了")
