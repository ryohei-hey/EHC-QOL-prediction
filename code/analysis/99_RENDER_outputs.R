# =============================================================================
# 99_RENDER_outputs.R  論文用の図表を作る唯一のファイル
# -----------------------------------------------------------------------------
# 目的 : 02–07 が output/derived/ に保存したデータだけを読み、論文用の図と表を作る。
#        ここでは計算をしない (見た目を変えるだけなら、このファイルだけを実行し直せばよい)。
# 入力 : output/derived/*.rds
# 出力 : output/figures/  Figure 1–3, eFigure 1–4 (PNG と PDF)
#        output/tables/   Table 1–2, eTable 1–10 (Word と、同じ表の本体の csv)、software.csv (R とパッケージの版)
#        output/figures/Figure1_footnote.txt, Figure3_footnote.txt, output/tables/Table2_footnote.txt
#                                       脚注に入れる数値 (IPW の値・イベントの内訳など)
#        output/tables/eMethods6_values.txt  eMethods 6 の本文に書く感度分析 S1・S5 の値
#        output/checks/diag_*.png       内部診断の図 (非公表)
# 対応 : protocol §7.13 (図表の一覧・軸・出現順の原則)
# 見た目を変えるには: まず「0. スタイルの設定」を編集する。個々の図表は各セクションの関数を編集する
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
suppressPackageStartupMessages({
  library(patchwork)   # 図を上下に並べる (Figure 2 の較正曲線 + ヒストグラム)
  library(flextable)   # Word の表
  library(officer)     # Word ファイルの書き出し
  library(grid)        # eFigure 2 のフロー図 (consort は R/flow_helpers.R で読み込む)
})

# =============================================================================
# 0. スタイルの設定 (全図表で共通。ここを変えれば全体が変わる)
# =============================================================================
# JAMA Network の配色 (ggsci::pal_jama: #374E55 #DF8F44 #00A1D5 #B24745 #79AF97 #6A6599 #80796B)
JAMA_COLS <- substr(ggsci::pal_jama()(7), 1, 7)
STYLE <- list(
  # モデルの色は全図で固定する (§7.13 (d))。JAMA Intern Med 投稿に合わせて JAMA の配色にした (2026-09-25)
  model_colors = c(checkup = JAMA_COLS[1], direct = JAMA_COLS[2], checkup_direct = JAMA_COLS[3],
                   questionnaire = JAMA_COLS[4]),
  # 開発と検証は点の形で区別する
  cohort_shapes = c("Development" = 16, "Temporal validation" = 17),
  # 開発 (optimism 補正) は実線、外部検証は破線 (Figure 2)
  cohort_linetypes = c("Development" = "solid", "Temporal validation" = "dashed"),
  auc_limits  = c(0.5, 1.0),          # AUC の軸 (Figure 1・3・eFigure 3 で共通)
  auc_refs    = c(0.7, 0.8),          # 参照線 (記述の目安。判定には使わない)
  calib_limit = 0.8,                  # 較正曲線の軸の上限 (0–80%)
  base_size   = 9,                    # 文字の大きさ (pt)
  font        = "Arial",              # 名前で指定する ("sans" は EMF にそのまま書かれ、Word が別の書体に置き換える)
  width_2col  = 174,                  # 図の幅 (mm)。2 段幅
  dpi         = 600,
  # eFigure は編集部が描き直さず、そのまま公開される。JAMA の規定 (Online-Only Figures) は
  # 「図の中の文字は Arial 10 pt、グラフはベクター形式で Word に挿入」。
  # Supplement には実寸で貼る。pandoc は reference_supplement.docx (用紙サイズの指定がない) の本文幅を
  # 5.83 インチ (148 mm) とみなし、それより広い図を縮める (縮むと文字が 10 pt 未満になる) ので、148 mm で作る
  efig_width  = 148,                  # eFigure の幅 (mm)
  efig_text   = 10                    # eFigure の文字の大きさ (pt)。軸の目盛・凡例・パネル名もすべてこの大きさ
)
# フロー図の大きさは pdf(NULL) で文字の幅を測って決める (R/flow_helpers.R)。pdf() は "Arial" を知らないので、
# 字幅が同じ Helvetica を Arial の名前で登録しておく (登録しないと警告が大量に出る)
if (is.null(grDevices::pdfFonts()[[STYLE$font]])) {
  do.call(grDevices::pdfFonts, setNames(list(grDevices::pdfFonts()$Helvetica), STYLE$font))
}
MODEL_LABELS <- c(checkup = "Checkup-based model", questionnaire = "Questionnaire-based model",
                  direct = "HRQOL-based model", checkup_direct = "Combined model")
OUTCOME_LABELS <- c(pcs = "PCS composite", mcs = "MCS composite")
DOMAIN_OF <- function(y) if_else(str_detect(y, "pcs"), "pcs", "mcs")
TIME_OF   <- function(y) if_else(str_detect(y, "18m"), "18m", "6m")

# 変数の表示名 (表で使う)
VAR_LABELS <- c(
  age = "Age, years", sex = "Sex",
  health_stts = "Self-rated health (Q1)", mental_stts = "Life satisfaction (Q2)",
  eat_habit = "Eats three meals a day (Q3)", chew = "Chewing difficulty (Q4)",
  swallow = "Choking (Q5)", bwchange = "Weight loss ≥2 kg in 6 months (Q6)",
  gaitspeed = "Slower walking (Q7)", fall = "Fall in the past year (Q8)",
  exercise = "Walks for exercise weekly (Q9)", memory = "Memory complaints (Q10)",
  orient = "Disorientation to date (Q11)", smoke = "Smoking (Q12)",
  goout = "Goes out weekly (Q13)", socnw = "Regular contact with family or friends (Q14)",
  socsppt = "Has someone to consult when unwell (Q15)",
  bmi = "Body mass index, kg/m²", sbp = "Systolic blood pressure, mm Hg",
  dbp = "Diastolic blood pressure, mm Hg", hdl = "HDL cholesterol, mg/dL",
  ldl = "LDL cholesterol, mg/dL", raw_tg = "Triglycerides, mg/dL", log_tg = "Triglycerides (log mg/dL)",
  fasting = "Time since last meal", raw_ast = "AST, U/L", log_ast = "AST (log U/L)",
  raw_alt = "ALT, U/L", log_alt = "ALT (log U/L)", raw_ggtp = "γ-GTP, U/L",
  log_ggtp = "γ-GTP (log U/L)", hba1c = "HbA1c (NGSP), %",
  egfr = "eGFR, mL/min/1.73 m²", upro = "Urine protein", uglu = "Urine glucose",
  pcs_pre = "Pre-checkup SF-8 PCS, score", mcs_pre = "Pre-checkup SF-8 MCS, score"
)

theme_paper <- function(base_size = STYLE$base_size) {
  theme_bw(base_size = base_size, base_family = STYLE$font) +
    theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"),
          legend.position = "bottom", legend.title = element_blank())
}
# eFigure 用: theme_bw は目盛・凡例・パネル名を本文の 0.8 倍にするので、すべて efig_text にそろえる
theme_efig <- function() {
  txt <- element_text(size = STYLE$efig_text)
  theme_paper(STYLE$efig_text) +
    theme(axis.text = txt, strip.text = txt, legend.text = txt, plot.title = txt)
}
save_fig <- function(p, name, height_mm, width_mm = STYLE$width_2col) {
  ggsave(file.path(DIR_FIGURES, paste0(name, ".png")), p, width = width_mm, height = height_mm,
         units = "mm", dpi = STYLE$dpi, bg = "white")
  ggsave(file.path(DIR_FIGURES, paste0(name, ".pdf")), p, width = width_mm, height = height_mm,
         units = "mm", device = cairo_pdf)
  message("図: ", name)
}
# EMF (ベクター) を書き出す。Supplement (Word) にはこれを貼る。draw は描画する関数
# 非 ASCII のパスで保存に失敗しないよう、一時ファイルに描いてからコピーする
write_emf <- function(name, width_mm, height_mm, draw) {
  tmp <- tempfile(name, fileext = ".emf")
  devEMF::emf(tmp, width = width_mm / 25.4, height = height_mm / 25.4, family = "Arial", bg = "white")
  draw(); dev.off()
  stopifnot(file.copy(tmp, file.path(DIR_FIGURES, paste0(name, ".emf")), overwrite = TRUE))
  unlink(tmp)
}
# eFigure: PNG と PDF (確認用・レポート用) に加えて EMF を書き出す
save_efig <- function(p, name, height_mm, width_mm = STYLE$efig_width) {
  save_fig(p, name, height_mm, width_mm)
  write_emf(name, width_mm, height_mm, \() print(p))
}
save_tables <- function(tabs, name, landscape = FALSE, widths = NULL) {
  # widths (インチ) を渡すと、自動の列幅の代わりにその幅を使う (CI の折り返しを防ぎたい表で使う)
  # 複数の表を 1 つの Word ファイルに順に書き出す
  doc <- read_docx()
  for (i in seq_along(tabs)) {
    # 表題は太字の本文段落にする (officer 既定の "heading 2" には "1.1" の自動番号がつくため使わない)
    if (!is.null(names(tabs)[i]) && nzchar(names(tabs)[i]))
      doc <- body_add_fpar(doc, fpar(ftext(names(tabs)[i], fp_text(bold = TRUE, font.size = 11, font.family = "Arial"))))
    # 列幅を内容に合わせ、ページの有効幅 (縦 6.3 インチ / 横 9.0 インチ) を超える場合は
    # 列幅を比例して縮める。文字の大きさは変えず、セルの中で折り返す
    ft <- if (is.null(widths)) autofit(tabs[[i]]) else width(tabs[[i]], j = seq_along(widths), width = widths)
    w <- dim(ft)$widths
    max_w <- if (landscape) 9.0 else 6.3
    if (sum(w) > max_w) ft <- width(ft, j = seq_along(w), width = w * max_w / sum(w))
    doc <- body_add_flextable(doc, ft)
    doc <- body_add_par(doc, "")
    # 同じ表の本体を csv にも書く (結果レポートの check チャンクが、本文に書いた数値をこれと照合する)。
    # 列名は flextable の列の名前、値は表に出る文字列のまま
    csv <- if (length(tabs) == 1) paste0(name, ".csv") else sprintf("%s_%d.csv", name, i)
    readr::write_excel_csv(ft$body$dataset[, ft$col_keys, drop = FALSE], file.path(DIR_TABLES, csv), na = "")
  }
  if (landscape) doc <- body_end_section_landscape(doc)
  print(doc, target = file.path(DIR_TABLES, paste0(name, ".docx")))
  message("表: ", name)
}
# 解析を回した R とパッケージの版 (結果レポートの Methods に書く)
local({
  pk <- c("glmnet", "pROC", "WeightedROC", "pmsampsize", "tidyverse", "gtsummary", "flextable", "officer",
          "ggplot2", "patchwork", "consort", "summarytools")
  ver <- vapply(pk, \(p) tryCatch(as.character(packageVersion(p)), error = \(e) NA_character_), "")
  readr::write_excel_csv(tibble(software = c("R", pk), version = c(paste(R.version$major, R.version$minor, sep = "."), ver)),
                         file.path(DIR_TABLES, "software.csv"))
})
# JAMA 形式 (AMA Manual of Style): 範囲はハイフン "0.74-0.80"。下限か上限が負のときは "−0.05 to 0.30"
jama_range <- function(lo, hi, fmt) {
  ifelse(lo < 0 | hi < 0, paste(sprintf(fmt, lo), "to", sprintf(fmt, hi)),
         paste0(sprintf(fmt, lo), "-", sprintf(fmt, hi)))
}
fmt_ci <- function(est, lo, hi, digits = 2) {
  f <- paste0("%.", digits, "f")
  paste0(sprintf(f, est), " (", jama_range(lo, hi, f), ")")
}
fmt_pct_ci <- function(est, lo, hi) fmt_ci(100 * est, 100 * lo, 100 * hi, digits = 1)
# JAMA 形式の整数: 4 桁以下は区切りなし (1478)、5 桁以上は細いスペース (10 340)
jama_int <- function(x) {
  s <- formatC(x, format = "d", big.mark = " ")
  ifelse(abs(x) < 10000, gsub(" ", "", s, fixed = TRUE), s)
}
# gtsummary の統計量の文字列を JAMA 形式にする: "1,483 (56.9%)" -> "1483 (56.9)"、
# "79.6 (77.1, 83.2)" -> "79.6 (77.1-83.2)"
jama_stat <- function(x) {
  replace_na(as.character(x), "") |>
    str_replace_all("\\d{1,3}(,\\d{3})+", \(m) jama_int(as.numeric(gsub(",", "", m)))) |>
    str_replace_all("%\\)", ")") |>
    str_replace_all("\\(([0-9.]+), ([0-9.]+)\\)", "(\\1-\\2)")
}
# 表の文字の手直し: 負の数の符号をマイナス記号 (−) にし、"-0.00" は "0.00" にする
fix_minus <- function(x) {
  x |> str_replace_all("(?<![\\w.])-(?=\\d)", "−") |> str_replace_all("−(0(\\.0+)?)(?![0-9.])", "\\1")
}
# 水準名の ASCII 表記 (">=10 h", "+/-", "-") を記号にする
pretty_level <- function(x) {
  x |> str_replace_all(">=", "≥") |> str_replace_all("\\+/-", "±") |> str_replace("^(\\s*)-$", "\\1−")
}
# 表の区切り線 (ブロックの境目に引く細い線) と、表全体にまたがる見出し行
thin_rule <- fp_border(width = 0.5, color = "grey60")
span_rows <- function(ft, i) {
  for (k in i) ft <- merge_at(ft, i = k, j = seq_len(ncol_keys(ft)))
  ft |> bold(i = i, part = "body") |> bg(i = i, bg = "grey92", part = "body")
}
ABBREV_NOTE <- "Abbreviations: AUC, area under the receiver operating characteristic curve; HRQOL, health-related quality of life; MCS, mental component summary; PCS, physical component summary; PPV, positive predictive value."
ABBREV_NOTE_NOPPV <- "Abbreviations: AUC, area under the receiver operating characteristic curve; HRQOL, health-related quality of life; MCS, mental component summary; PCS, physical component summary."
# 略語の定義の一覧。表ごとに、その表で使う略語だけを並べた Abbreviations 行を abbrev_note() で作る
ABBREV_DEF <- c(
  "γ-GTP" = "γ-glutamyltransferase", ALT = "alanine aminotransferase", AST = "aspartate aminotransferase",
  AUC = "area under the receiver operating characteristic curve", "ΔAUC" = "difference in AUC",
  Cr = "serum creatinine", eGFR = "estimated glomerular filtration rate", FY = "fiscal year",
  HbA1c = "hemoglobin A1c", HDL = "high-density lipoprotein", HRQOL = "health-related quality of life",
  LDL = "low-density lipoprotein", MCS = "mental component summary",
  NGSP = "National Glycohemoglobin Standardization Program", PCS = "physical component summary",
  QMCOO = "Questionnaire for Medical Checkup of Old-Old", SD = "standard deviation",
  PPV = "positive predictive value", "SF-8" = "8-item Short-Form Health Survey")
abbrev_note <- function(...) {
  keys <- c(...)
  stopifnot(all(keys %in% names(ABBREV_DEF)))
  paste0("Abbreviations: ", paste(keys, ABBREV_DEF[keys], sep = ", ", collapse = "; "), ".")
}
# 検査値の略語と SI 換算 (eTable 1・eTable 3。Table 1 は summary_flextable に同じ内容がある)
LAB_ABBREV_E1 <- abbrev_note("γ-GTP", "ALT", "AST", "Cr", "eGFR", "HbA1c", "HDL", "LDL", "NGSP", "QMCOO")
LAB_ABBREV_E3 <- abbrev_note("γ-GTP", "ALT", "AST", "eGFR", "HbA1c", "HDL", "LDL", "MCS", "NGSP", "PCS", "QMCOO", "SD")
SI_NOTE <- "SI conversion factors: To convert HDL and LDL cholesterol to mmol/L, multiply by 0.0259; triglycerides to mmol/L, multiply by 0.0113; AST, ALT, and γ-GTP to μkat/L, multiply by 0.0167; HbA1c to proportion of total hemoglobin, multiply by 0.01; serum creatinine to μmol/L, multiply by 88.4; eGFR to mL/s/1.73 m², multiply by 0.0167."
# eTable 3 にはクレアチニンの行がないので、その換算を除いた版
SI_NOTE_NOCR <- sub("serum creatinine to μmol/L, multiply by 88.4; ", "", SI_NOTE, fixed = TRUE)
stopifnot(SI_NOTE_NOCR != SI_NOTE)
# 変数の区分 (Table 1・eTable 3・eTable 2 で区分の見出し行を入れる)
VAR_SECTION <- c(
  setNames(rep("Demographics", 2), c("age", "sex")),
  setNames(rep("Questionnaire for Medical Checkup of Old-Old (QMCOO)", 15),
           c("health_stts", "mental_stts", "eat_habit", "chew", "swallow", "bwchange", "gaitspeed", "fall",
             "exercise", "memory", "orient", "smoke", "goout", "socnw", "socsppt")),
  setNames(rep("Checkup measurements", 17),
           c("bmi", "sbp", "dbp", "hdl", "ldl", "raw_tg", "log_tg", "fasting", "raw_ast", "log_ast",
             "raw_alt", "log_alt", "raw_ggtp", "log_ggtp", "hba1c", "egfr", "upro")),
  uglu = "Checkup measurements",
  setNames(rep("Pre-checkup SF-8", 2), c("pcs_pre", "mcs_pre"))
)
# 区分が変わる行の前に見出し行を入れる。見出し行の番号 (section_i) を属性で返す
add_sections <- function(df, var) {
  sec <- unname(VAR_SECTION[var])
  stopifnot(!anyNA(sec))
  blocks <- split(df, factor(sec, levels = unique(sec)))
  out <- imap_dfr(blocks, \(b, s) bind_rows(tibble(!!names(df)[1] := s), b)) |>
    mutate(across(everything(), \(x) replace_na(as.character(x), "")))
  attr(out, "section_i") <- which(out[[1]] %in% names(blocks) & rowSums(out[-1] != "") == 0)
  out
}
# JAMA 形式の脚注記号: 上付きの a, b ... (記号 † などは使わない)
sup_header <- function(ft, j, label, mark = "a") {
  compose(ft, i = nrow_part(ft, "header"), j = j, part = "header",
          value = as_paragraph(label, as_sup(mark)))
}
sup_footer <- function(ft, i, text = ft$footer$dataset[[1]][i], mark = "a") {
  compose(ft, i = i, j = 1, part = "footer", value = as_paragraph(as_sup(mark), text))
}
write_footnote <- function(lines, name, dir = DIR_FIGURES) {
  writeLines(lines, file.path(dir, paste0(name, "_footnote.txt")), useBytes = TRUE)
}

# 時点の表示名は、実データの間隔 (02 の eFigure 1 のデータ) から作る (小数第 1 位に四捨五入, §6.2)
tl <- read_derived("efig1_timeline")$timeline
rng <- function(s) { x <- filter(tl, survey == s); sprintf("%.1f–%.1f y", min(x$min), max(x$max)) }
TIME_LABELS <- c("6m" = paste0("≈6 months\n(", rng("6m"), ")"),
                 "18m" = paste0("≈18 months\n(", rng("18m"), ")"))
# 表の列ラベル・セルでは時点名だけを使い、間隔の幅は脚注 (TIME_NOTE) に書く
TIME_SHORT <- c("6m" = "≈6 months", "18m" = "≈18 months")
time_note <- function(times = c("6m", "18m")) {
  paste0("Actual intervals from the checkup to the outcome assessment: ",
         paste0(TIME_SHORT[times], ", ", map_chr(times, rng), collapse = "; "), ".")
}
# 表のアウトカム名 (PCS composite / MCS composite) と本文の呼び方 (主・副次アウトカム) の対応を全表の脚注に書く (2026-10-10)
OUTCOME_NOTE <- "PCS composite (primary outcome): physical component summary (PCS) score <40, care-need level ≥3, or death. MCS composite (secondary outcome): mental component summary (MCS) score <40, care-need level ≥3, or death."

# 読み込み
perf_dev <- read_derived("perf_dev"); perf_ext <- read_derived("perf_ext")
sens <- read_derived("sens"); flow <- read_derived("efig2_flow")
# 結果レポートが本文の人数を照合するため、フロー図の人数 (集計値) を output/tables にも置く
file.copy(file.path(DIR_DERIVED, "efig2_flow_counts.csv"), file.path(DIR_TABLES, "eFigure2_counts.csv"), overwrite = TRUE)

# derived が TEST_MODE (bootstrap の回数を減らした動作確認) で作られていたら警告する。
# 論文には TEST_MODE <- FALSE で作り直した図表を使うこと
meta <- attr(perf_dev, "meta")
if (isTRUE(meta$test_mode)) {
  warning("derived は TEST_MODE (N_BOOT = ", meta$n_boot, ") で作られています。論文用には本番の設定で作り直してください",
          call. = FALSE)
}

# =============================================================================
# Table 1. 背景特性
# =============================================================================
# gtsummary の table_body を、表示名つき・区分の見出し行つきの表にする (Table 1・eTable 2 で共通)
summary_rows <- function(body, stat_cols) {
  body <- body |>
    mutate(label = case_when(
      row_type == "label" & var_type == "dichotomous" ~ paste0(VAR_LABELS[variable], ", yes"),
      # 連続変数は「名前, median (IQR), 単位」(JAMA 形式。表の既定は No. (%))
      row_type == "label" & str_detect(var_type, "continuous") ~
        str_replace(unname(VAR_LABELS[variable]), "^(.*), ([^,]+)$", "\\1, median (IQR), \\2"),
      row_type == "label" ~ unname(VAR_LABELS[variable]),
      .default = paste0("    ", pretty_level(label))))
  # 二値の項目で「はい」がリスクの低い回答のもの (Q3・Q9・Q13–15) も、表示は「yes」の割合にそろえている
  body <- body |> mutate(across(all_of(stat_cols), jama_stat))
  # 整数で記録される測定値 (血圧・脂質・肝酵素など) は、行の値がすべて ".0" で終わるときは小数を付けない ("136 (127-144)")
  int_row <- str_detect(body$var_type, "continuous") %in% TRUE &
    pmap_lgl(body[stat_cols], \(...) all(str_detect(unlist(str_extract_all(c(...), "\\d+\\.\\d+")), "\\.0$")))
  body <- body |> mutate(across(all_of(stat_cols), \(x) if_else(int_row, str_replace_all(x, "(\\d+)\\.0(?!\\d)", "\\1"), x)))
  add_sections(body |> select(label, all_of(stat_cols)), body$variable)
}
# 健診前の SF-8 の時期 (Table 1・eTable 2 の脚注)
PRE_SF8_NOTE <- "Pre-checkup SF-8: scores in the most recent March survey before the checkup that included the SF-8 (0.2 to 1.8 years before the checkup; eMethods 2)."
summary_flextable <- function(out) {
  sec_i <- attr(out, "section_i")
  flextable(out) |> span_rows(sec_i) |>
    align(j = seq(2, ncol(out)), align = "center", part = "all") |>
    add_footer_lines(c("Abbreviations: γ-GTP, γ-glutamyltransferase; ALT, alanine aminotransferase; AST, aspartate aminotransferase; eGFR, estimated glomerular filtration rate; HbA1c, hemoglobin A1c; HDL, high-density lipoprotein; IQR, interquartile range; LDL, low-density lipoprotein; MCS, mental component summary; NGSP, National Glycohemoglobin Standardization Program; PCS, physical component summary; QMCOO, Questionnaire for Medical Checkup of Old-Old; SF-8, 8-item Short-Form Health Survey.",
                       "SI conversion factors: To convert HDL and LDL cholesterol to mmol/L, multiply by 0.0259; triglycerides to mmol/L, multiply by 0.0113; AST, ALT, and γ-GTP to μkat/L, multiply by 0.0167; HbA1c to proportion of total hemoglobin, multiply by 0.01; eGFR to mL/s/1.73 m², multiply by 0.0167.",
                       PRE_SF8_NOTE)) |>
    fontsize(size = 8, part = "all") |> font(fontname = "Arial", part = "all")
}
make_summary_table <- function(body, n, stat_cols, header_names) {
  out <- summary_rows(body, stat_cols)
  sec_i <- attr(out, "section_i")
  names(out) <- c("Characteristic", paste0(header_names, "\n(n = ", jama_int(n), ")"))
  attr(out, "section_i") <- sec_i
  # JAMA 形式: 値は No. (%) が既定 (連続変数は行ラベルに median (IQR) と書く)
  summary_flextable(out) |>
    add_header_row(values = c("Characteristic", "No. (%)"), colwidths = c(1, ncol(out) - 1)) |>
    merge_v(j = 1, part = "header") |> align(i = 1, part = "header", align = "center") |>
    fontsize(size = 8, part = "all") |> font(fontname = "Arial", part = "all")
}
tbl1 <- read_derived("tbl1")
save_tables(list(`Table 1. Characteristics of the development and temporal validation cohorts` =
                   make_summary_table(tbl1$body, tbl1$n, c("stat_0", "stat_1", "stat_2"),
                                      c("Overall", "Development", "Temporal validation"))),
            "Table1")

# =============================================================================
# Figure 1. 健診モデルの判別能 (約 1 年半後)
# =============================================================================
fig1_data <- bind_rows(
  perf_dev$perf |> filter(model == "checkup", stat == "auc", str_detect(outcome, "18m")) |>
    transmute(outcome, cohort = "Development", est = corrected, lo, hi),
  perf_ext$perf |> filter(model == "checkup", stat == "auc", set == "V", str_detect(outcome, "18m")) |>
    transmute(outcome, cohort = "Temporal validation", est = estimate, lo, hi)
) |>
  mutate(domain = factor(OUTCOME_LABELS[DOMAIN_OF(outcome)], levels = OUTCOME_LABELS),
         cohort = factor(cohort, levels = names(STYLE$cohort_linetypes)))
# ROC 曲線 (開発は見かけの曲線、外部検証は V)。パネル内に AUC (95% CI) を書く
# (開発の AUC は optimism 補正の値。曲線そのものは補正できないため見かけのまま)
roc_curves <- read_derived("roc_curves")
fig1_roc <- roc_curves |>
  filter(model == "checkup", set %in% c("D", "V")) |>
  mutate(domain = factor(OUTCOME_LABELS[DOMAIN_OF(outcome)], levels = OUTCOME_LABELS),
         cohort = factor(cohort, levels = names(STYLE$cohort_linetypes)))
# 凡例は Figure 3 と同じ形で、パネルごとに「線種 コホート名 AUC (95% CI)」の 2 行をパネル右下に置く
fig1_key <- fig1_data |>
  mutate(y = 0.05 + 0.075 * (nlevels(cohort) - as.integer(cohort)),
         name = as.character(cohort),
         auc = paste0("AUC ", fmt_ci(est, lo, hi)))
fig1 <- ggplot(fig1_roc, aes(fpr, tpr, linetype = cohort)) +
  geom_abline(intercept = 0, slope = 1, colour = "grey70", linewidth = 0.3) +
  geom_step(colour = STYLE$model_colors[["checkup"]], linewidth = 0.5, direction = "hv") +
  # 凡例の後ろに白い背景を敷く (対角線と重ならないように)
  annotate("rect", xmin = 0.18, xmax = 1, ymin = 0.01, ymax = max(fig1_key$y) + 0.045,
           fill = "white", colour = NA) +
  geom_segment(data = fig1_key, aes(x = 0.20, xend = 0.27, y = y, yend = y),
               colour = STYLE$model_colors[["checkup"]], linewidth = 0.6) +
  geom_text(data = fig1_key, aes(x = 0.29, y = y, label = name), inherit.aes = FALSE,
            hjust = 0, size = 2.3) +
  geom_text(data = fig1_key, aes(x = 0.98, y = y, label = auc), inherit.aes = FALSE,
            hjust = 1, size = 2.3) +
  facet_wrap(~domain) +
  scale_linetype_manual(values = STYLE$cohort_linetypes, guide = "none") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), expand = expansion(mult = 0.01)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), expand = expansion(mult = 0.01)) +
  coord_equal() +
  # 予測の時点と、健診からの間隔の幅は脚注に書く (§6.2)
  labs(x = "1 − Specificity", y = "Sensitivity") +
  theme_paper() +
  theme(panel.spacing = unit(5, "mm"))   # 隣のパネルの目盛 (1.0 と 0.0) が重ならないように
save_fig(fig1, "Figure1", height_mm = 92)
# 脚注の数値: イベントの内訳 (排他的な分類: 低 HRQOL > 要介護 3 以上 > 死亡) と、外部検証の IPW の AUC (S1)
fo <- flow$outcomes |> filter(time == "18m", set %in% c("Development", "Validation"))
s1v <- sens$s1_ext |> filter(quantity == "ipw.auc")
write_footnote(c(
  paste0("Outcome at ", str_replace(TIME_LABELS[["18m"]], "\n", " "), " after the checkup."),
  "Development AUC is optimism-corrected (bootstrap); the development ROC curve is the apparent curve of the final model in the development cohort. Temporal validation AUC is for the final model (DeLong 95% CI).",
  "The gray diagonal line indicates no discrimination (AUC 0.5).",
  "Values without correction are the performance among participants with a known outcome.",
  pmap_chr(fo, \(set, domain, n_known, events, low, ltc3, death, ...) sprintf(
    "%s, %s: %d events among %d with known outcome (poor HRQOL %d, care-need level ≥3 %d, death %d; mutually exclusive, poor HRQOL first).",
    set, toupper(domain), events, n_known, low, ltc3, death)),
  pmap_chr(s1v, \(outcome, estimate, lo, hi, ...) sprintf(
    "Inverse probability weighted AUC, temporal validation, %s: %s.", toupper(DOMAIN_OF(outcome)),
    fmt_ci(estimate, lo, hi)))
), "Figure1")

# =============================================================================
# Figure 2. 健診モデルの較正曲線 (約 1 年半後)
# =============================================================================
calib <- bind_rows(
  read_derived("calib_dev") |> filter(str_detect(outcome, "18m")) |>
    transmute(outcome, p, observed = corrected, cohort = "Development"),
  read_derived("calib_ext") |> filter(set == "V", model == "checkup") |>
    transmute(outcome, p, observed, cohort = "Temporal validation")
) |>
  mutate(domain = factor(OUTCOME_LABELS[DOMAIN_OF(outcome)], levels = OUTCOME_LABELS)) |>
  filter(!is.na(observed))
hist_d <- bind_rows(
  read_derived("pred_hist_dev") |> filter(model == "checkup", str_detect(outcome, "18m")) |>
    mutate(cohort = "Development"),
  read_derived("pred_hist_ext") |> filter(model == "checkup", set == "V") |> mutate(cohort = "Temporal validation")
) |>
  # 全体に対する割合を先に計算してから、表示範囲 (0–80%) の外を除く
  group_by(outcome, cohort) |> mutate(prop = count / sum(count)) |> ungroup() |>
  filter(mid <= STYLE$calib_limit) |>
  # 開発は上向き、検証は下向き (向かい合わせのヒストグラム)
  mutate(prop = if_else(cohort == "Development", prop, -prop),
         domain = factor(OUTCOME_LABELS[DOMAIN_OF(outcome)], levels = OUTCOME_LABELS))
lim <- c(0, STYLE$calib_limit)
# 凡例は Figure 1・3 と同じ形で、パネルごとに「線種 コホート名」の 2 行を右下 (対角線より下の空いた所) に置く
# 線の長さは破線の模様が 2 周期以上見える長さにする
fig2_key <- tidyr::expand_grid(domain = factor(OUTCOME_LABELS, levels = OUTCOME_LABELS),
                               cohort = names(STYLE$cohort_linetypes)) |>
  mutate(y = STYLE$calib_limit * (0.03 + 0.07 * (cohort == "Development")),
         name = c(Development = "Development (optimism-corrected)",
                  `Temporal validation` = "Temporal validation (final model)")[cohort])
fig2_x <- STYLE$calib_limit * c(seg0 = 0.33, seg1 = 0.42, txt = 0.44)
fig2_top <- ggplot(calib, aes(p, observed, linetype = cohort)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(colour = STYLE$model_colors[["checkup"]], linewidth = 0.7) +
  geom_segment(data = fig2_key, aes(x = fig2_x[["seg0"]], xend = fig2_x[["seg1"]], y = y, yend = y),
               colour = STYLE$model_colors[["checkup"]], linewidth = 0.7) +
  geom_text(data = fig2_key, aes(x = fig2_x[["txt"]], y = y, label = name), inherit.aes = FALSE,
            hjust = 0, size = 2.3) +
  facet_wrap(~domain) +
  scale_linetype_manual(values = STYLE$cohort_linetypes, guide = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = lim, ylim = lim) +   # 範囲外の線は切るだけで、データは落とさない
  labs(x = NULL, y = "Observed proportion") + theme_paper() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
fig2_bottom <- ggplot(hist_d, aes(mid, prop, fill = cohort)) +
  geom_col(width = 0.018) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  facet_wrap(~domain) +
  scale_fill_manual(values = c("Development" = "grey40", "Temporal validation" = "grey70"), guide = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = \(x) scales::percent(abs(x))) +
  coord_cartesian(xlim = lim) +
  labs(x = "Predicted risk", y = "Share") + theme_paper() +
  theme(strip.text = element_blank(), strip.background = element_blank())
fig2 <- fig2_top / fig2_bottom + plot_layout(heights = c(3, 1))
save_fig(fig2, "Figure2", height_mm = 105)

# =============================================================================
# Table 2. 高リスク者の抽出 (約 1 年半後、外部検証)
# =============================================================================
tbl2 <- read_derived("tbl2_highrisk")
s1e4 <- sens$s1_ext |> filter(str_starts(quantity, "e4")) |>
  separate(quantity, into = c("x", "model", "group", "frac", "measure"), sep = "\\|") |>
  mutate(frac = as.numeric(frac))
# eTable 6 と同じ形: 2 つのモデルを左右に並べ、同じ行 (アウトカム × 対象集団 × 抽出割合) で比べる。
# 対象集団の人数・結果確定者数と抽出した人数はモデルによらないので、行の見出し側に書く。
# 非抽出群の人数と無作為抽出の期待値は本文の表には載せない (2026-09-23 決定。
# 査読で求められたら tbl2_highrisk.rds の n_unsel*・selected_prop・prevalence から出す)
stopifnot(tbl2 |> group_by(outcome, group, frac) |>
            summarise(k = n_distinct(n_sel, n_sel + n_unsel, n_sel_unknown + n_unsel_unknown), .groups = "drop") |>
            pull(k) |> max() == 1)
big <- jama_int
tbl2_out <- tbl2 |>
  arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")), match(group, c("all", "pre_not_low")), frac) |>
  mutate(Outcome = OUTCOME_LABELS[DOMAIN_OF(outcome)],
         # 結果が分かっている人ではなく、結果が欠測の人数を示す (読者には欠測のほうが分かりやすい)
         Population = sprintf("%s\nn = %s\n(outcome unknown, %s)\nOutcome, %.1f%%",
                              if_else(group == "all", "All eligible", "Pre-checkup score ≥40"),
                              big(n_sel + n_unsel), big(n_sel_unknown + n_unsel_unknown), 100 * prevalence),
         Top = sprintf("Top %d%%\n(n = %s)", round(100 * frac), big(n_sel)),
         sens = fmt_pct_ci(sensitivity, sens_lo, sens_hi), ppv = fmt_pct_ci(ppv, ppv_lo, ppv_hi),
         missing = big(n_sel_unknown),
         # 最大感度: 選ばれた結果確定者が全員イベントだった場合 (選ばれた結果確定者数 ÷ イベント数、上限 100%)。
         # 選ばれる人はモデルで異なるため、モデルごとの値になる
         max = sprintf("%.1f", 100 * pmin(1, n_sel_known / ev_total))) |>
  select(Outcome, Population, Top, model, sens, max, ppv, missing) |>
  pivot_wider(names_from = model, values_from = c(sens, max, ppv, missing), names_glue = "{model}_{.value}") |>
  select(Outcome, Population, Top, checkup_sens, checkup_max, checkup_ppv, checkup_missing,
         direct_sens, direct_max, direct_ppv, direct_missing)
t2_model_hdr <- list(sens = "Sensitivity, %\n(95% CI)", max = "Maximum sensitivity, %",
                     ppv = "PPV, %\n(95% CI)", missing = "Selected with\noutcome unknown, No.")
# 列見出しへの注 (上付き a: 抽出の定義、b: 最大感度)
T2_NOTE_A <- "Participants in the top 10%, 20%, or 30% of predicted risk among all eligible participants in the population, regardless of whether the outcome was known; ties at the threshold were all included."
T2_NOTE_B <- "Sensitivity if every selected participant with a known outcome had the outcome (selected participants with known outcomes divided by participants with the outcome, capped at 100%); it differs by model because the selected participants differ."
ft2 <- flextable(tbl2_out) |>
  set_header_labels(values = c(
    list(Top = "Selected as high risk"),
    setNames(t2_model_hdr, paste0("checkup_", names(t2_model_hdr))),
    setNames(t2_model_hdr, paste0("direct_", names(t2_model_hdr))))) |>
  add_header_row(values = c("", MODEL_LABELS[["checkup"]], MODEL_LABELS[["direct"]]), colwidths = c(3, 4, 4)) |>
  merge_v(j = c("Outcome", "Population")) |> valign(j = 1:2, valign = "top", part = "body") |>
  align(j = 3:11, align = "center", part = "all") |> align(i = 1, align = "center", part = "header") |>
  hline(i = seq(3, nrow(tbl2_out) - 1, by = 3), border = thin_rule) |>
  # JAMA の順: Abbreviations → 一般の注 → 上付き a, b (列見出しへの注)
  add_footer_lines(c(
    abbrev_note("HRQOL", "MCS", "PCS", "PPV"),
    "Sensitivity: proportion of participants with the outcome who were selected. PPV: proportion of selected participants who had the outcome. Both were estimated among participants with known outcomes; 95% CIs: bootstrap percentile (stratified by fiscal year; threshold recomputed in each sample).",
    "Outcome, %: proportion with the outcome among participants with known outcomes (the PPV expected with random selection).",
    "Pre-checkup score ≥40: PCS score (PCS composite) or MCS score (MCS composite) of 40 or higher in the survey before the checkup.",
    OUTCOME_NOTE, time_note("18m"),
    T2_NOTE_A,
    T2_NOTE_B
  )) |>
  sup_header("Top", "Selected as high risk") |>
  sup_header("checkup_max", t2_model_hdr$max, mark = "b") |> sup_header("direct_max", t2_model_hdr$max, mark = "b") |>
  (\(ft) { f <- ft$footer$dataset[[1]]
           sup_footer(sup_footer(ft, match(T2_NOTE_A, f)), match(T2_NOTE_B, f), mark = "b") })() |>
  fontsize(size = 7.5, part = "all") |> font(fontname = "Arial", part = "all") |>
  padding(padding.top = 1.5, padding.bottom = 1.5, part = "all") |>
  # 対象集団ごとの 3 行のブロックが、ページをまたいで切れないようにする
  paginate(group = "Population", group_def = "rle")
save_tables(list(`Table 2. Identification of high-risk participants in the temporal validation cohort (outcome at ≈18 months)` = ft2),
            "Table2", landscape = TRUE, widths = c(0.65, 1.45, 0.65, rep(c(1.0, 0.65, 1.0, 0.75), 2)))
write_footnote(pmap_chr(s1e4, \(model, group, frac, measure, estimate, lo, hi, outcome, ...) sprintf(
  "IPW (S1) %s, %s, %s, top %d%%: %s %s%%", toupper(DOMAIN_OF(outcome)), model, group, round(100 * frac),
  measure, fmt_pct_ci(estimate, lo, hi))), "Table2", dir = DIR_TABLES)

# =============================================================================
# Figure 3. 構成要素ごとの判別能 (探索的解析 E9、外部検証、約 1 年半後)
# =============================================================================
# 2026-09-29 変更: 以前はモデル比較の ROC 曲線 (E5–E7、約半年後・約 1 年半後) だった。
# Key message (健診データは要介護・死亡を捉えるが、低い HRQOL の判別は HRQOL を加えると上がる) に
# 直接対応する、構成要素ごとの AUC と ΔAUC (統合 − 健診, E7) のフォレストプロットに差し替えた。
# モデル比較の AUC・ΔAUC は eTable 4・eTable 9 に、構成要素ごとの値は eTable 10 にある
fig3_models <- c("checkup", "direct", "checkup_direct")
FIG3_COMPONENTS <- c(composite = "Composite outcome", low_hrqol = "(a) Poor HRQOL among survivors",
                     ltc3_death = "(b) Care-need level ≥3 or death")
# 行の位置: 上から複合・(a)・(b)。同じ行の中でモデルを上下にずらす
fig3_pos <- function(d) d |>
  mutate(domain = factor(paste0("Model for the ", OUTCOME_LABELS[DOMAIN_OF(outcome)]),
                         levels = paste0("Model for the ", OUTCOME_LABELS)),
         y = match(component, rev(names(FIG3_COMPONENTS))))
fig3_auc <- bind_rows(
  perf_ext$perf |> filter(set == "V", stat == "auc", outcome %in% c("y_pcs_18m", "y_mcs_18m"),
                          model %in% fig3_models) |> mutate(component = "composite"),
  perf_ext$comp |> filter(set == "V", stat == "auc", model %in% fig3_models)
) |>
  fig3_pos() |>
  mutate(model = factor(model, levels = fig3_models),
         y = y + c(0.22, 0, -0.22)[as.integer(model)])
fig3_delta <- bind_rows(
  perf_ext$delta |> filter(set == "V", contrast == "E7", outcome %in% c("y_pcs_18m", "y_mcs_18m")) |>
    mutate(component = "composite"),
  perf_ext$comp_delta |> filter(set == "V", contrast == "E7")
) |> fig3_pos()
stopifnot(nrow(fig3_auc) == 2 * 3 * length(fig3_models), nrow(fig3_delta) == 2 * 3)
fig3_y <- scale_y_continuous(breaks = 1:3, labels = rev(FIG3_COMPONENTS), limits = c(0.6, 3.4), expand = c(0, 0))
fig3_left <- ggplot(fig3_auc, aes(estimate, y, colour = model)) +
  geom_vline(xintercept = STYLE$auc_refs, colour = "grey75", linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.12, orientation = "y") +
  geom_point(size = 1.6) +
  facet_grid(domain ~ .) +
  scale_colour_manual(values = STYLE$model_colors[fig3_models], labels = MODEL_LABELS[fig3_models]) +
  scale_x_continuous(limits = STYLE$auc_limits) + fig3_y +
  labs(x = "AUC (95% CI)", y = NULL, title = "Discrimination in temporal validation") +
  theme_paper() +
  theme(plot.title = element_text(size = STYLE$base_size), strip.text.y = element_blank())
d3_rng <- range(c(fig3_delta$lo, fig3_delta$hi, 0))
fig3_right <- ggplot(fig3_delta, aes(estimate, y)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.12, orientation = "y",
                colour = STYLE$model_colors[["checkup_direct"]]) +
  geom_point(size = 1.6, colour = STYLE$model_colors[["checkup_direct"]]) +
  facet_grid(domain ~ .) +
  scale_x_continuous(limits = d3_rng + c(-0.01, 0.01),
                     labels = scales::label_number(accuracy = 0.01, style_negative = "minus")) + fig3_y +
  labs(x = "ΔAUC (95% CI)", y = NULL, title = "Combined − checkup-based model") +
  theme_paper() +
  theme(plot.title = element_text(size = STYLE$base_size), axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
fig3 <- fig3_left + fig3_right + plot_layout(widths = c(1.4, 1), guides = "collect") &
  theme(legend.position = "bottom")
save_fig(fig3, "Figure3", height_mm = 120)
# 脚注の数値: 構成要素ごとの対象者数とイベント数 (モデルによらない)
fig3_n <- fig3_auc |> filter(model == "checkup") |> arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")), desc(y))
write_footnote(c(
  "Exploratory analysis (E9). Models fitted for the composite outcome at ≈18 months, evaluated for each component without refitting, in the temporal validation cohort (DeLong 95% CI; paired DeLong for ΔAUC).",
  pmap_chr(fig3_n, \(outcome, component, n, events, ...) sprintf("%s, %s: %d events among %d.",
    toupper(DOMAIN_OF(outcome)), FIG3_COMPONENTS[[component]], events, n)),
  "(a) Participants alive without care-need level ≥3 who had an SF-8 score; outcome, SF-8 score <40. (b) All participants with a known composite outcome.",
  "In (a), the HRQOL-based and combined models share the SF-8 measurement with the outcome.",
  time_note("18m")
), "Figure3")

# =============================================================================
# eTable 1. 予測因子の定義
# =============================================================================
etbl1 <- read_derived("etbl1_predictors") |>
  mutate(predictor = recode(predictor, "Regular meals (Q3)" = "Eats three meals a day (Q3)",
                            "Slow walking (Q7)" = "Slower walking (Q7)", "Falls (Q8)" = "Fall in the past year (Q8)"),
         response = str_replace(response, "^mmHg$", "mm Hg"))
ft_e1 <- flextable(etbl1 |> rename(Domain = domain, Predictor = predictor,
                                   `Question wording or measurement` = wording,
                                   `Response categories or unit` = response,
                                   `Coding in the model` = coding, Source = source)) |>
  merge_v(j = "Domain") |> valign(j = 1, valign = "top", part = "body") |>
  # 領域の境目に細い線を引く
  hline(i = which(head(etbl1$domain, -1) != tail(etbl1$domain, -1)), border = thin_rule) |>
  add_footer_lines(c("Ref, reference category (for questionnaire items, the lowest-risk response). Predictors were standardized before penalization in ridge regression.",
                     "QMCOO wording follows the official English version (eMethods 1); the Japanese wording is that of the Ministry of Health, Labour and Welfare questionnaire. Total: 39 parameters.",
                     LAB_ABBREV_E1, SI_NOTE)) |>
  fontsize(size = 7.5, part = "all") |> font(fontname = "Arial", part = "all")
save_tables(list(`eTable 1. Definition of predictors` = ft_e1), "eTable1", landscape = TRUE)

# =============================================================================
# eTable 3. 健診モデルの係数
# =============================================================================
# 列名 (例 "health_sttsGood") を表示名 (例 "Self-rated health (Q1): Good") に直す
label_coef <- function(nm, vars) {
  map_chr(nm, \(x) {
    if (x == "(Intercept)") return("Intercept")
    v <- vars[startsWith(x, vars)]; v <- v[which.max(nchar(v))]
    lev <- substring(x, nchar(v) + 1)
    if (nzchar(lev)) paste0(VAR_LABELS[[v]], ": ", lev) else VAR_LABELS[[v]]
  })
}
final <- read_derived("final_models")
coef_var <- function(nm, vars) {
  map_chr(nm, \(x) { v <- vars[startsWith(x, vars)]; if (length(v)) v[which.max(nchar(v))] else NA_character_ })
}
etbl2 <- imap_dfr(final, \(fm, yv) {
  f <- fm$checkup
  tibble(term = names(f$coef), coef = f$coef, coef_std = c(NA, f$coef_std),
         label = pretty_level(label_coef(names(f$coef), f$vars)), var = coef_var(names(f$coef), f$vars),
         outcome = yv, lambda = f$lambda)
})
# 列は PCS → MCS、各アウトカムの中で ≈18 months (主解析) → ≈6 months の順に並べる
E2_COLS <- c("y_pcs_18m", "y_pcs_6m", "y_mcs_18m", "y_mcs_6m")
stopifnot(setequal(unique(etbl2$outcome), E2_COLS))
etbl2_wide <- etbl2 |>
  mutate(value = fix_minus(if_else(term == "(Intercept)", sprintf("%.4f", coef),
                                   sprintf("%.4f [%.2f]", coef, coef_std)))) |>
  select(term, var, label, outcome, value) |>
  pivot_wider(names_from = outcome, values_from = value) |>
  select(term, var, label, all_of(E2_COLS))
lambda_row <- etbl2 |> distinct(outcome, lambda) |>
  mutate(value = sprintf("%.4g", lambda)) |> select(outcome, value) |>
  pivot_wider(names_from = outcome, values_from = value) |> mutate(label = "Selected λ (penalty)")
e2_body <- bind_rows(
  etbl2_wide |> filter(term == "(Intercept)") |> select(label, all_of(E2_COLS)),
  add_sections(etbl2_wide |> filter(term != "(Intercept)") |> select(label, all_of(E2_COLS)),
               etbl2_wide$var[etbl2_wide$term != "(Intercept)"]),
  lambda_row |> select(label, all_of(E2_COLS))
)
e2_sec <- which(e2_body$label %in% unique(VAR_SECTION) & rowSums(e2_body[-1] != "") == 0)
ft_e2 <- flextable(e2_body) |>
  set_header_labels(label = "Predictor", y_pcs_18m = TIME_SHORT[["18m"]], y_pcs_6m = TIME_SHORT[["6m"]],
                    y_mcs_18m = TIME_SHORT[["18m"]], y_mcs_6m = TIME_SHORT[["6m"]]) |>
  add_header_row(values = c("", OUTCOME_LABELS[["pcs"]], OUTCOME_LABELS[["mcs"]]), colwidths = c(1, 2, 2)) |>
  span_rows(e2_sec) |>
  hline(i = c(1, nrow(e2_body) - 1), border = thin_rule) |>
  align(j = 2:5, align = "center", part = "all") |>
  add_footer_lines(c(
    "Values are log-odds coefficients per unit of the predictor in original units, reported to 4 decimal places so that the model can be applied [standardized log-odds coefficient in brackets: per SD of the predictor in the development data, for continuous predictors and dummy variables alike]. The predicted risk is 1/(1 + exp(−LP)), where LP is the intercept plus the sum of coefficients multiplied by predictor values; triglycerides, AST, ALT, and γ-GTP enter as natural logarithms.",
    "Standardized coefficients indicate relative contribution to prediction and are not causal effects; contributions are shared among correlated laboratory values.",
    "The ≈18-month models are the main analysis; the ≈6-month models are exploratory.",
    LAB_ABBREV_E3, SI_NOTE_NOCR,
    OUTCOME_NOTE, time_note(c("18m", "6m"))
  )) |>
  fontsize(size = 7.5, part = "all") |> font(fontname = "Arial", part = "all")
save_tables(list(`eTable 3. Coefficients of the checkup-based model (ridge regression)` = ft_e2), "eTable3", landscape = TRUE)

# =============================================================================
# eTable 4・9・6. 予測性能の詳細 (判別と較正 / ΔAUC / 高リスク者の抽出の開発での見かけの値)
# =============================================================================
boot_dist <- read_derived("boot_dist")
slope_dist <- boot_dist |> group_by(outcome, model) |>
  summarise(slope_boot = fmt_ci(median(slope_test, na.rm = TRUE),
                                 quantile(slope_test, 0.025, na.rm = TRUE), quantile(slope_test, 0.975, na.rm = TRUE)),
            lambda_boot = sprintf("%.3g (%.3g-%.3g)", median(lambda, na.rm = TRUE),
                                  quantile(lambda, 0.025, na.rm = TRUE), quantile(lambda, 0.975, na.rm = TRUE)),
            .groups = "drop")
# 最終モデルで選ばれた λ (§7.5 で報告する)
lambda_final <- imap_dfr(final, \(fm, yv) imap_dfr(fm, \(f, m) tibble(outcome = yv, model = m, lambda_final = f$lambda)))
dev_rows <- perf_dev$perf |>
  mutate(value = if_else(stat == "auc", fmt_ci(corrected, lo, hi), sprintf("%.2f", corrected))) |>
  select(outcome, model, stat, value, optimism) |>
  pivot_wider(names_from = stat, values_from = c(value, optimism)) |>
  left_join(slope_dist, by = c("outcome", "model")) |>
  left_join(lambda_final, by = c("outcome", "model")) |>
  left_join(perf_dev$perf |> distinct(outcome, model, n, events), by = c("outcome", "model")) |>
  # 列は「性能 (開発と検証で共通)」→「開発の bootstrap の詳細」の順に並べる
  transmute(outcome, model, cohort = "Development (optimism-corrected)",
            `Events/participants, No.` = sprintf("%d/%d", events, n), AUC = value_auc,
            `Calibration slope` = value_slope, `Calibration-in-the-large` = value_citl,
            `AUC optimism` = sprintf("%.3f", optimism_auc),
            `Slope in bootstrap samples` = slope_boot,
            `λ, final model` = sprintf("%.3g", lambda_final),
            `λ in bootstrap samples` = lambda_boot)
# 外部検証の集合の表示名 (eTable 4 の行・eTable 9 の列で共通)
VAL_SET_LABELS <- c(V        = "Temporal validation (FY2022 and FY2023 checkups combined)",
                    V_FY2022 = "Temporal validation, FY2022 checkup only",
                    V_FY2023 = "Temporal validation, FY2023 checkup only",
                    V_6m     = "Temporal validation at ≈6 months (FY2023 checkup)")
# eTable 4 の行と eTable 9 の列に、健診前の HRQOL が低くなかった人での値を足す (2026-10-11。Table 2 の同じ集団に対応)
E3_SET_LABELS <- c(VAL_SET_LABELS, V_pre40 = "Temporal validation, pre-checkup score ≥40")
# 開発の location-shifted CI が点推定を中心にしない理由 (eTable 4・9・10 の脚注で共通、2026-10-11)。
# bootstrap 標本での見かけの値は、パラメータの多いモデルほど元のデータより大きく膨らむ (03 の boot_dist で確認)
LS_CI_NOTE <- "Development 95% CIs are location-shifted bootstrap intervals (eMethods 5): the 2.5th and 97.5th percentiles of the apparent values in bootstrap samples minus the estimated optimism. Because apparent values in bootstrap samples differed systematically from those in the original data (the apparent AUC was inflated more for models with more parameters), these intervals are not centered on the optimism-corrected estimates."
ext_rows <- perf_ext$perf |>
  filter(set %in% names(E3_SET_LABELS)) |>
  mutate(value = fmt_ci(estimate, lo, hi)) |>
  select(outcome, model, set, stat, value, n, events) |>
  pivot_wider(names_from = stat, values_from = value) |>
  transmute(outcome, model, cohort = E3_SET_LABELS[set],
            `Events/participants, No.` = sprintf("%d/%d", events, n), AUC = auc, `Calibration slope` = slope, `Calibration-in-the-large` = citl)
e3a <- bind_rows(dev_rows, ext_rows) |>
  mutate(Outcome = paste0(OUTCOME_LABELS[DOMAIN_OF(outcome)], ", ", TIME_SHORT[TIME_OF(outcome)]),
         Model = MODEL_LABELS[model],
         # 主たる結果 = 健診モデルの約 1 年半後 (primary・E1–E3)。約半年後 (E6) と他のモデルは探索的解析
         Analysis = if_else(model == "checkup" & TIME_OF(outcome) == "18m",
                            "Main results (primary, E1–E3)", "Exploratory (E5–E8)")) |>
  arrange(Analysis != "Main results (primary, E1–E3)", match(TIME_OF(outcome), c("18m", "6m")),
          match(DOMAIN_OF(outcome), c("pcs", "mcs")), match(model, names(MODEL_LABELS)), cohort) |>
  select(Analysis, Outcome, Model, Cohort = cohort, everything(), -outcome, -model) |>
  # 開発だけの列 (bootstrap の詳細) は、検証の行では空欄にせず "—" (該当なし) とする
  mutate(across(c(`AUC optimism`, `Slope in bootstrap samples`, `λ, final model`, `λ in bootstrap samples`),
                \(x) replace_na(x, "—"))) |>
  mutate(across(everything(), \(x) fix_minus(replace_na(as.character(x), ""))))
# 解析の区分 (主解析 / 探索的解析) は列ではなく、表全体にまたがる見出し行にする
e3_body <- split(e3a, factor(e3a$Analysis, levels = unique(e3a$Analysis))) |>
  map_dfr(\(d) bind_rows(tibble(Outcome = recode(d$Analysis[1],
                                                 "Main results (primary, E1–E3)" = "Primary and secondary outcomes",
                                                 "Exploratory (E5–E8)" = "Exploratory analyses")),
                         select(d, -Analysis))) |>
  mutate(across(everything(), \(x) replace_na(x, "")))
e3_sec <- which(e3_body$Model == "")
# モデル (アウトカム × モデル) の境目に細い線を引く
e3_key <- paste(e3_body$Outcome, e3_body$Model)
e3_key[e3_sec] <- NA
e3_key <- Reduce(\(a, b) if (is.na(b)) a else b, e3_key, accumulate = TRUE)
e3_rule <- setdiff(which(head(e3_key, -1) != tail(e3_key, -1)), c(e3_sec, e3_sec - 1))
# 検証でイベントが 100 未満の行は慎重に解釈する印をつける (eMethods 6、2026-10-10。2026-10-13 に「参考値」から変更)
e3_ev  <- suppressWarnings(as.integer(sub("/.*", "", e3_body$`Events/participants, No.`)))
e3_ref <- which(e3_body$Cohort != "Development (optimism-corrected)" & !is.na(e3_ev) & e3_ev < 100)
ft_e3 <- flextable(e3_body) |>
  set_header_labels(`Slope in bootstrap samples` = "Calibration slope in bootstrap samples",
                    `λ in bootstrap samples` = "λ in bootstrap samples") |>
  add_header_row(values = c("", "Performance", "Development cohort only: bootstrap internal validation"),
                 colwidths = c(4, 3, 4)) |>
  align(i = 1, align = "center", part = "header") |>
  merge_v(j = c("Outcome", "Model")) |> valign(j = 1:3, valign = "top", part = "body") |>
  hline(i = e3_rule, border = thin_rule) |>
  span_rows(e3_sec) |>
  add_footer_lines(c(
    "Values in parentheses are 95% CIs unless otherwise indicated. Development values are optimism-corrected by bootstrap; temporal validation values are for the final models fitted in the development cohort (DeLong 95% CI for the AUC; Wald 95% CIs for calibration). Calibration in development is shown without CIs (eMethods 5). AUC optimism: mean difference between the AUC of each bootstrap model in its bootstrap sample and in the original development data.",
    "Median (2.5th to 97.5th percentile) across bootstrap samples.",
    "Fewer than 100 events in validation; interpret with caution (eMethods 6).",
    "Calibration slope <1 indicates predictions that are too extreme, >1 predictions that are too moderate; calibration-in-the-large >0 indicates that risk was underestimated on average. —, not applicable (development cohort only).",
    LS_CI_NOTE,
    "Pre-checkup score ≥40: participants with a PCS score (PCS composite) or MCS score (MCS composite) of 40 or higher in the survey before the checkup (the population also shown in Table 2).",
    abbrev_note("AUC", "FY", "HRQOL", "MCS", "PCS"), "λ indicates the penalty parameter of the ridge regression.",
    OUTCOME_NOTE, time_note()
  )) |>
  sup_header("Slope in bootstrap samples", "Calibration slope in bootstrap samples") |>
  sup_header("λ in bootstrap samples", "λ in bootstrap samples") |>
  sup_footer(2, "Median (2.5th to 97.5th percentile) across bootstrap samples.") |>
  compose(i = e3_ref, j = "Events/participants, No.", part = "body",
          value = as_paragraph(e3_body$`Events/participants, No.`[e3_ref], as_sup("b"))) |>
  sup_footer(3, "Fewer than 100 events in validation; interpret with caution (eMethods 6).", mark = "b")
delta_rows <- bind_rows(
  perf_dev$delta |> transmute(outcome, contrast, cohort = "Development (optimism-corrected)",
                              value = fmt_ci(corrected, lo, hi, 3)),
  perf_ext$delta |> filter(set %in% names(E3_SET_LABELS)) |>
    transmute(outcome, contrast, cohort = E3_SET_LABELS[set], value = fmt_ci(estimate, lo, hi, 3))
) |>
  mutate(Outcome = paste0(OUTCOME_LABELS[DOMAIN_OF(outcome)], ", ", TIME_SHORT[TIME_OF(outcome)]),
         Contrast =recode(contrast, E5 = "HRQOL-based − checkup-based", E7 = "Combined − checkup-based",
                           E7_ref = "Combined − HRQOL-based (reference)", E8 = "Checkup-based − questionnaire-based")) |>
  select(Outcome, Contrast, cohort, value) |>
  pivot_wider(names_from = cohort, values_from = value) |>
  select(Outcome, Contrast, `Development (optimism-corrected)`, all_of(unname(E3_SET_LABELS))) |>
  # 該当しない組み合わせ (≈18 months の年度別と ≈6 months) は "—" と書く
  mutate(across(everything(), \(x) fix_minus(replace_na(x, "—"))))
# 検証の列は「Temporal validation」を上の見出し行にまとめ、下の行には集合の違いだけを書く
e4_val_sub <- c(V = "FY2022 and FY2023\ncheckups combined", V_FY2022 = "FY2022 checkup only",
                V_FY2023 = "FY2023 checkup only", V_6m = "At ≈6 months\n(FY2023 checkup)",
                V_pre40 = "Pre-checkup score ≥40\n(FY2022 and FY2023)")
stopifnot(identical(names(e4_val_sub), names(E3_SET_LABELS)))
ft_e4 <- flextable(delta_rows) |>
  set_header_labels(values = setNames(as.list(e4_val_sub), unname(E3_SET_LABELS))) |>
  set_header_labels(`Development (optimism-corrected)` = "Development\n(optimism-corrected)") |>
  add_header_row(values = c("", "", "", "Temporal validation"), colwidths = c(1, 1, 1, 5)) |>
  align(i = 1, align = "center", part = "header") |> align(j = 3:8, align = "center", part = "all") |>
  merge_v(j = "Outcome") |> valign(j = 1, valign = "top", part = "body") |>
  hline(i = which(head(delta_rows$Outcome, -1) != tail(delta_rows$Outcome, -1)), border = thin_rule) |>
  add_footer_lines(c(
    "Values are ΔAUC (95% CI). ΔAUC >0 indicates a higher AUC for the first-named model. Temporal validation 95% CIs are paired DeLong intervals.",
    LS_CI_NOTE,
    "Pre-checkup score ≥40: participants with a PCS score (PCS composite) or MCS score (MCS composite) of 40 or higher in the survey before the checkup (the population also shown in Table 2).",
    "—, not applicable.",
    abbrev_note("AUC", "ΔAUC", "FY", "HRQOL", "MCS", "PCS"),
    OUTCOME_NOTE, time_note()
  ))
# 2 つのモデルを左右に並べ、同じ行 (アウトカム × 対象集団 × 抽出割合) で比べられる形にする
e4dev_raw <- read_derived("e4_dev_apparent")
# 対象集団の人数と結果の割合 (無作為に抽出したときの PPV) はモデルによらないので、集団のセルに書く
stopifnot(e4dev_raw |> group_by(outcome, group, frac) |>
            summarise(k = n_distinct(prevalence, n_sel + n_unsel), .groups = "drop") |> pull(k) |> max() == 1)
e4dev <- e4dev_raw |>
  arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")), match(group, c("all", "pre_not_low")), frac) |>
  mutate(Outcome = OUTCOME_LABELS[DOMAIN_OF(outcome)],
         Population = sprintf("%s\nn = %s\nOutcome, %.1f%%",
                              if_else(group == "all", "All eligible", "Pre-checkup score ≥40"),
                              jama_int(n_sel + n_unsel), 100 * prevalence),
         Top = sprintf("Top %d%%", round(100 * frac)),
         sens = sprintf("%.1f", 100 * sensitivity), ppv = sprintf("%.1f", 100 * ppv)) |>
  select(Outcome, Population, Top, model, sens, ppv) |>
  pivot_wider(names_from = model, values_from = c(sens, ppv), names_glue = "{model}_{.value}") |>
  select(Outcome, Population, Top, checkup_sens, checkup_ppv, direct_sens, direct_ppv)
ft_e5 <- flextable(e4dev) |>
  set_header_labels(Population = "Population", Top = "Selected as high risk",
                    checkup_sens = "Sensitivity, %", checkup_ppv = "PPV, %",
                    direct_sens = "Sensitivity, %", direct_ppv = "PPV, %") |>
  add_header_row(values = c("", MODEL_LABELS[["checkup"]], MODEL_LABELS[["direct"]]), colwidths = c(3, 2, 2)) |>
  merge_v(j = c("Outcome", "Population")) |> valign(j = 1:2, valign = "top", part = "body") |>
  align(j = 3:7, align = "center", part = "all") |> align(i = 1, align = "center", part = "header") |>
  hline(i = seq(3, nrow(e4dev) - 1, by = 3), border = thin_rule) |>
  # Table 2 と同じ順: Abbreviations → 一般の注 → 上付き a (抽出の定義は Table 2 の T2_NOTE_A と同じ)
  add_footer_lines(c(
    abbrev_note("HRQOL", "MCS", "PCS", "PPV"),
    "Apparent performance: the final models were applied to the same development data on which they were fitted, so the values are expected to be optimistic. Performance in the temporal validation cohort is shown in Table 2.",
    "Sensitivity: proportion of participants with the outcome who were selected. PPV: proportion of selected participants who had the outcome. Both were calculated among participants with a known outcome. Outcome, %: proportion with the outcome among those with a known outcome (the PPV expected if participants were selected at random).",
    "Pre-checkup score ≥40: PCS score (PCS composite) or MCS score (MCS composite) of 40 or higher in the survey before the checkup.",
    OUTCOME_NOTE, time_note("18m"),
    T2_NOTE_A
  )) |>
  sup_header("Top", "Selected as high risk") |>
  (\(ft) { i <- match(T2_NOTE_A, ft$footer$dataset[[1]]); stopifnot(!is.na(i)); sup_footer(ft, i) })()
# 文字の大きさと書体は、脚注・見出し行を足したあとに最後にかける (先にかけると足した行が既定の 11 pt になる)
tf <- \(ft) ft |> fontsize(size = 7, part = "all") |> font(fontname = "Arial", part = "all")
# 感度分析 S1・S5 の値を照合用のテキストファイルにまとめる (output/tables/eMethods6_values.txt)。
# S1 の詳細は eTable 7 に示す (2026-10-10 変更。以前は eMethods の本文)
# S1 (IPW): 固定した最終モデルの、重みなし (結果確定者、見かけ) と重みつきの AUC・較正 (§7.7 S1)
s1_rows <- bind_rows(sens$s1_dev, sens$s1_ext) |>
  filter(str_detect(quantity, "^(unw|ipw)\\.(auc|slope|citl)$")) |>
  separate(quantity, into = c("weighting", "stat"), sep = "\\.") |>
  transmute(line = sprintf("S1, %s, %s, %s, %s: %s",
                           if_else(set == "D", "development (apparent)", "temporal validation"),
                           OUTCOME_LABELS[DOMAIN_OF(outcome)],
                           if_else(weighting == "unw", "unweighted (known outcome)", "inverse probability weighted"),
                           recode(stat, auc = "AUC", slope = "calibration slope", citl = "calibration-in-the-large"),
                           fmt_ci(estimate, lo, hi)))
# S5: 罰則なし + 一様縮小。縮小係数、optimism 補正 AUC、E5・E8 の ΔAUC、縮小後のモデルの外部検証での較正 (§7.7 S5)
s5 <- sens$s5
s5_rows <- bind_rows(
  s5$perf |> filter(stat == "auc") |>
    transmute(line = paste0("S5, AUC, ", MODEL_LABELS[model], " (development, optimism-corrected): ",
                            fmt_ci(corrected, lo, hi))),
  s5$delta |> transmute(line = paste0("S5, ΔAUC ", contrast, " (development, optimism-corrected): ",
                                      fmt_ci(corrected, lo, hi, 3))),
  s5$ext |> filter(stat != "auc") |>
    transmute(line = paste0("S5, ", recode(stat, slope = "calibration slope", citl = "calibration-in-the-large"),
                            ", ", MODEL_LABELS[model], " (temporal validation; shrinkage factor ",
                            sprintf("%.2f", shrinkage), "): ", fmt_ci(estimate, lo, hi)))
)
writeLines(c(s1_rows$line, s5_rows$line), file.path(DIR_TABLES, "eMethods6_values.txt"), useBytes = TRUE)
save_tables(list(`eTable 4. Discrimination and calibration` = tf(ft_e3)),
            "eTable4", landscape = TRUE, widths = c(0.8, 0.8, 1.5, 0.95, 0.95, 1.0, 0.55, 1.05, 0.55, 0.9))
save_tables(list(`eTable 9. Differences in AUC between models (ΔAUC, 95% CI)` = tf(ft_e4)),
            "eTable9", landscape = TRUE, widths = c(1.0, 1.6, 1.15, rep(1.1, 5)))
save_tables(list(`eTable 6. Identification of high-risk participants in the development cohort (apparent performance; outcome at ≈18 months)` = tf(ft_e5)),
            "eTable6")

# =============================================================================
# eTable 7. 感度分析 S1 (IPW) の詳細
# =============================================================================
# 2026-10-10 変更: 以前は eMethods の本文に書いていた (2026-09-23 決定) が、eMethods を簡潔にするため表に移した。
# 表 1: 固定した最終モデルの、重みなし (結果確定者、見かけ) と重みつきの AUC・較正 (開発と外部検証)
# 表 2: 外部検証での高リスク者の抽出の重みつきの感度・PPV (重みなしの値は Table 2)
S1_STATS <- c(auc = "AUC", slope = "Calibration slope", citl = "Calibration-in-the-large")
s1_tab <- bind_rows(sens$s1_dev, sens$s1_ext) |>
  filter(str_detect(quantity, "^(unw|ipw)\\.(auc|slope|citl)$")) |>
  separate(quantity, into = c("weighting", "stat"), sep = "\\.") |>
  mutate(Outcome = OUTCOME_LABELS[DOMAIN_OF(outcome)],
         Cohort = if_else(set == "D", "Development (apparent)", "Temporal validation"),
         Measure = S1_STATS[stat], value = fix_minus(fmt_ci(estimate, lo, hi))) |>
  arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")), match(set, c("D", "V")), match(stat, names(S1_STATS))) |>
  select(Outcome, Cohort, Measure, weighting, value) |>
  pivot_wider(names_from = weighting, values_from = value)
ft_s1 <- flextable(s1_tab, col_keys = c("Outcome", "Cohort", "Measure", "unw", "ipw")) |>
  set_header_labels(unw = "Unweighted (95% CI)", ipw = "Inverse probability weighted (95% CI)") |>
  merge_v(j = c("Outcome", "Cohort")) |> valign(j = 1:2, valign = "top", part = "body") |>
  align(j = 4:5, align = "center", part = "all") |>
  hline(i = seq(3, nrow(s1_tab) - 1, by = 3), border = thin_rule) |>
  add_footer_lines(c(
    "The final models were applied without refitting. Unweighted: participants with known outcomes; in development, this is the apparent performance of the final model.",
    "Inverse probability weighted: participants whose outcome was determined by registry events had a weight of 1; survey respondents without registry events, the inverse of their estimated probability of response (truncated at the 99th percentile); nonrespondents and all participants who relocated, including those with an event before relocation, 0 (eMethods 11).",
    "95% CIs: bootstrap percentile intervals from 1000 samples (stratified by fiscal year in temporal validation), repeating the response model and truncation in each sample. Unweighted values in temporal validation therefore have slightly different 95% CIs from those in eTable 4 (DeLong for the AUC, Wald for calibration).",
    "Abbreviations: AUC, area under the receiver operating characteristic curve; MCS, mental component summary; PCS, physical component summary.",
    OUTCOME_NOTE, time_note("18m")
  ))
s1_sel <- s1e4 |>
  mutate(Outcome = OUTCOME_LABELS[DOMAIN_OF(outcome)],
         Population = if_else(group == "all", "All eligible", "Pre-checkup score ≥40"),
         Top = sprintf("Top %d%%", round(100 * frac)), value = fix_minus(fmt_pct_ci(estimate, lo, hi))) |>
  arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")), match(group, c("all", "pre_not_low")), frac) |>
  select(Outcome, Population, Top, model, measure, value) |>
  pivot_wider(names_from = c(model, measure), values_from = value, names_glue = "{model}_{measure}") |>
  select(Outcome, Population, Top, checkup_sensitivity, checkup_ppv, direct_sensitivity, direct_ppv)
ft_s1_sel <- flextable(s1_sel) |>
  set_header_labels(Top = "Selected as high risk",
                    checkup_sensitivity = "Sensitivity, % (95% CI)", checkup_ppv = "PPV, % (95% CI)",
                    direct_sensitivity = "Sensitivity, % (95% CI)", direct_ppv = "PPV, % (95% CI)") |>
  add_header_row(values = c("", MODEL_LABELS[["checkup"]], MODEL_LABELS[["direct"]]), colwidths = c(3, 2, 2)) |>
  merge_v(j = c("Outcome", "Population")) |> valign(j = 1:2, valign = "top", part = "body") |>
  align(j = 3:7, align = "center", part = "all") |> align(i = 1, align = "center", part = "header") |>
  hline(i = seq(3, nrow(s1_sel) - 1, by = 3), border = thin_rule) |>
  add_footer_lines(c(
    "Temporal validation cohort. Selection as high risk as in Table 2 (top 10%, 20%, or 30% of predicted risk among all eligible participants in the population); unweighted values are shown in Table 2.",
    "Sensitivity and PPV were weighted as above. 95% CIs: bootstrap percentile intervals from 1000 samples stratified by fiscal year.",
    "Pre-checkup score ≥40: PCS score (PCS composite) or MCS score (MCS composite) of 40 or higher in the survey before the checkup.",
    "Abbreviations: HRQOL, health-related quality of life; MCS, mental component summary; PCS, physical component summary; PPV, positive predictive value.",
    OUTCOME_NOTE, time_note("18m")
  ))
save_tables(list(`eTable 7A. Discrimination and calibration with inverse probability weighting (sensitivity analysis S1)` = tf(ft_s1),
                 `eTable 7B. Identification of high-risk participants with inverse probability weighting in temporal validation` = tf(ft_s1_sel)),
            "eTable7", landscape = TRUE)

# =============================================================================
# eTable 2. 結果が確定した人と結果不明の人の比較
# =============================================================================
etbl4 <- read_derived("etbl4")
# 開発と外部検証を横に並べて 1 つの表にする (行は変数・水準で対応させる)
e6_key <- c("variable", "row_type", "label")
stopifnot(identical(etbl4$D$body[e6_key], etbl4$V$body[e6_key]))
e6_body <- etbl4$D$body |>
  rename(D_known = stat_1, D_unknown = stat_2) |>
  bind_cols(etbl4$V$body |> select(V_known = stat_1, V_unknown = stat_2)) |>
  summary_rows(c("D_known", "D_unknown", "V_known", "V_unknown"))
e6_n <- \(s, g) paste0(g, " outcome\n(n = ", jama_int(etbl4[[s]]$n[[g]]), ")")
ft_e6 <- summary_flextable(e6_body) |>
  set_header_labels(label = "Characteristic",
                    D_known = e6_n("D", "Known"), D_unknown = e6_n("D", "Unknown"),
                    V_known = e6_n("V", "Known"), V_unknown = e6_n("V", "Unknown")) |>
  add_header_row(values = c("Characteristic", "Development, No. (%)", "Temporal validation, No. (%)"),
                 colwidths = c(1, 2, 2)) |>
  merge_v(j = 1, part = "header") |>
  align(i = 1, part = "header", align = "center") |>
  add_footer_lines("Unknown outcome at approximately 18 months: no SF-8 score and neither care-need level ≥3 nor death by March 31 of the outcome survey year, or relocation out of the city without a prior event (eMethods 10).") |>
  fontsize(size = 8, part = "all") |> font(fontname = "Arial", part = "all")
save_tables(list(`eTable 2. Participants with known and unknown outcomes (PCS composite, ≈18 months) in the development and temporal validation cohorts` = ft_e6),
            "eTable2")

# =============================================================================
# eTable 10. 複合アウトカムの構成要素ごとの判別能 (E9, §7.5)
# =============================================================================
# 約 1 年半後の PCS 複合・MCS 複合で推定したモデル (健診・健診前 HRQOL・統合) の AUC と ΔAUC を、
# 複合アウトカム (参考) と、2 つの構成要素 ((a) 低い HRQOL、(b) 要介護 3 以上・死亡) ごとに示す。
# 開発は同じ bootstrap 標本で optimism 補正した値、外部検証は V (年度を統合) の値
E9_MODELS <- c("checkup", "direct", "checkup_direct")
E9_CONTRASTS <- c(E5 = "ΔAUC, HRQOL-based − checkup-based", E7 = "ΔAUC, combined − checkup-based")
e9_comp_label <- function(comp, dom) {
  sc <- toupper(dom)
  recode(comp, composite = "Composite outcome (reference)",
         low_hrqol = paste0("(a) ", sc, " <40 among participants alive without care-need level ≥3 who had an SF-8 score"),
         ltc3_death = "(b) Care-need level ≥3 or death")
}
e9_auc <- bind_rows(
  perf_dev$perf |> filter(stat == "auc", outcome %in% c("y_pcs_18m", "y_mcs_18m"), model %in% E9_MODELS) |>
    transmute(outcome, component = "composite", row = model, cohort = "dev",
              value = fmt_ci(corrected, lo, hi), n, events),
  perf_dev$comp |> filter(model %in% E9_MODELS) |>
    transmute(outcome, component, row = model, cohort = "dev", value = fmt_ci(corrected, lo, hi), n, events),
  perf_ext$perf |> filter(set == "V", stat == "auc", outcome %in% c("y_pcs_18m", "y_mcs_18m"), model %in% E9_MODELS) |>
    transmute(outcome, component = "composite", row = model, cohort = "val",
              value = fmt_ci(estimate, lo, hi), n, events),
  perf_ext$comp |> filter(model %in% E9_MODELS) |>
    transmute(outcome, component, row = model, cohort = "val", value = fmt_ci(estimate, lo, hi), n, events)
)
e9_delta <- bind_rows(
  perf_dev$delta |> filter(contrast %in% names(E9_CONTRASTS), outcome %in% c("y_pcs_18m", "y_mcs_18m")) |>
    transmute(outcome, component = "composite", row = contrast, cohort = "dev", value = fmt_ci(corrected, lo, hi, 3)),
  perf_dev$comp_delta |> transmute(outcome, component, row = contrast, cohort = "dev",
                                   value = fmt_ci(corrected, lo, hi, 3)),
  perf_ext$delta |> filter(set == "V", contrast %in% names(E9_CONTRASTS), outcome %in% c("y_pcs_18m", "y_mcs_18m")) |>
    transmute(outcome, component = "composite", row = contrast, cohort = "val", value = fmt_ci(estimate, lo, hi, 3)),
  perf_ext$comp_delta |> transmute(outcome, component, row = contrast, cohort = "val",
                                   value = fmt_ci(estimate, lo, hi, 3))
)
# 外部検証の較正: 複合アウトカム (参考) は最終モデルの値、構成要素は換算前と換算後 (Gondrie らの方法)
e9_cal <- bind_rows(
  perf_ext$perf |> filter(set == "V", stat != "auc", outcome %in% c("y_pcs_18m", "y_mcs_18m"), model %in% E9_MODELS) |>
    transmute(outcome, component = "composite", row = model, col = paste0(stat, "_unadjusted"),
              value = fmt_ci(estimate, lo, hi)),
  perf_ext$comp_cal |> filter(model %in% E9_MODELS) |>
    transmute(outcome, component, row = model, col = paste0(stat, "_", version), value = fmt_ci(estimate, lo, hi))
) |> pivot_wider(names_from = col, values_from = value)
# 構成要素の対象の人数とイベント数 (モデルによらないので、構成要素のセルに書く)
e9_n <- e9_auc |> distinct(outcome, component, cohort, n, events)
stopifnot(!anyDuplicated(select(e9_n, outcome, component, cohort)))
e9_n <- e9_n |>
  mutate(txt = sprintf("%s n = %s (%s events)", if_else(cohort == "dev", "Development", "Validation"),
                       jama_int(n), jama_int(events))) |>
  group_by(outcome, component) |>
  summarise(n_txt = paste(txt[order(cohort)], collapse = "\n"), .groups = "drop") |>
  left_join(perf_ext$comp_k |> select(outcome, component, k), by = c("outcome", "component")) |>
  mutate(n_txt = if_else(is.na(k), n_txt, sprintf("%s\nConversion factor k = %.2f", n_txt, k)))
e9_body <- bind_rows(select(e9_auc, -n, -events), e9_delta) |>
  pivot_wider(names_from = cohort, values_from = value) |>
  left_join(e9_cal, by = c("outcome", "component", "row")) |>
  left_join(e9_n, by = c("outcome", "component")) |>
  arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")),
          match(component, c("composite", "low_hrqol", "ltc3_death")),
          match(row, c(E9_MODELS, names(E9_CONTRASTS)))) |>
  transmute(Outcome = paste0("Models for the ", OUTCOME_LABELS[DOMAIN_OF(outcome)]),
            Component = paste0(e9_comp_label(component, DOMAIN_OF(outcome)), "\n", n_txt),
            Model = if_else(row %in% names(E9_CONTRASTS), E9_CONTRASTS[row], MODEL_LABELS[row]),
            Development = dev, Validation = val,
            citl_u = citl_unadjusted, citl_a = citl_adapted, slope_u = slope_unadjusted, slope_a = slope_adapted) |>
  mutate(across(everything(), \(x) fix_minus(replace_na(x, "—"))))
stopifnot(nrow(e9_body) == 2 * 3 * (length(E9_MODELS) + length(E9_CONTRASTS)))
ft_e7 <- flextable(e9_body) |>
  set_header_labels(Outcome = "Model fitted for", Component = "Outcome evaluated", Model = "Model or contrast",
                    Development = "Development\n(optimism-corrected)", Validation = "Temporal validation",
                    citl_u = "Unadjusted", citl_a = "Converted", slope_u = "Unadjusted", slope_a = "Converted") |>
  add_header_row(values = c("", "AUC or ΔAUC", "Calibration-in-the-large\n(temporal validation)",
                            "Calibration slope\n(temporal validation)"), colwidths = c(3, 2, 2, 2)) |>
  align(i = 1, align = "center", part = "header") |>
  merge_v(j = c("Outcome", "Component")) |> valign(j = 1:2, valign = "top", part = "body") |>
  align(j = 4:9, align = "center", part = "all") |>
  hline(i = seq(5, nrow(e9_body) - 1, by = 5), border = thin_rule) |>
  add_footer_lines(c(
    "Values are AUC or ΔAUC (95% CI). ΔAUC >0 indicates a higher AUC for the first-named model.",
    "Models were fitted for the composite outcome at ≈18 months (poor HRQOL, care-need level ≥3, or death) and were not refitted for each component. Development values are optimism-corrected using the same bootstrap samples as for the composite outcome (location-shifted 95% CI); temporal validation values are for the final models (DeLong 95% CI; paired DeLong for ΔAUC).",
    LS_CI_NOTE,
    "Components were evaluated among participants with a known composite outcome. (a) Participants who had neither reached care-need level ≥3 nor died by March 31 of the outcome survey year and had an SF-8 score; the outcome was a score <40. This evaluation is conditional on survival, not reaching care-need level ≥3, and responding to the survey. (b) All participants with a known composite outcome; the outcome was care-need level ≥3 or death by March 31 of the outcome survey year.",
    "In component (a), the HRQOL-based and combined models share the SF-8 measurement with the outcome, which may favor these models.",
    "Temporal validation includes participants from both the FY2022 and FY2023 checkups. For the composite outcome, calibration is that of the final models. For the components, unadjusted calibration uses the predicted risk of the composite outcome as it is.",
    "Predicted risks converted to component-specific risks by the method of Gondrie et al (eMethods 13): risk = 1 − (1 − p)^k, where p is the predicted risk of the composite outcome and k is the incidence of the component divided by the incidence of the composite outcome among all participants with a known composite outcome in the development cohort (components counted whenever they occurred). The original method multiplies the baseline cumulative hazard of a Cox model by k; applying it to a logistic model is an approximation. Because converted risks refer to all participants, calibration for component (a) was assessed among all participants with a known composite outcome (outcome: SF-8 score <40; participants who reached care-need level ≥3 or died without such a score were counted as nonevents), whereas the AUC for (a) was assessed in the conditional population described above. Conversion does not change the AUC. Calibration was not assessed in the development cohort because k was estimated there.",
    "Calibration slope <1 indicates predictions that are too extreme, >1 predictions that are too moderate; calibration-in-the-large >0 indicates that risk was underestimated on average. —, not applicable.",
    abbrev_note("AUC", "ΔAUC", "FY", "HRQOL", "MCS", "PCS", "SF-8"),
    OUTCOME_NOTE, time_note("18m")
  )) |>
  fontsize(size = 7, part = "all") |> font(fontname = "Arial", part = "all") |>
  sup_header("citl_a", "Converted") |> sup_header("slope_a", "Converted") |>
  # 脚注 a (Converted の定義) は行番号ではなく本文で探す (脚注の行が増減してもずれないように)
  (\(ft) { i <- grep("^Predicted risks converted", ft$footer$dataset[[1]]); stopifnot(length(i) == 1)
           sup_footer(ft, i) })() |>
  fontsize(size = 7, part = "all") |> font(fontname = "Arial", part = "all")
save_tables(list(`eTable 10. Discrimination and calibration for each component of the composite outcome (≈18 months)` = ft_e7),
            "eTable10", landscape = TRUE, widths = c(0.75, 1.45, 1.3, 1.0, 1.0, 0.95, 0.95, 0.95, 0.95))

# =============================================================================
# eTable 8. 性別ごとの主たる結果 (外部検証、約 1 年半後。08_by_sex.R)
# =============================================================================
# JAMA Network の投稿規定 (主なアウトカムを性別ごとに示す) に合わせた記述的な提示。
# 抽出の閾値は男女合わせた全適格者で決め、感度と陽性的中率を性別ごとに集計する
by_sex <- read_derived("by_sex")
e8_perf <- by_sex$perf |>
  mutate(val = if_else(stat == "auc", fmt_ci(estimate, lo, hi), fmt_ci(estimate, lo, hi))) |>
  select(outcome, sex, stat, val, n, events) |>
  pivot_wider(names_from = stat, values_from = val)
e8_e4 <- by_sex$e4 |>
  mutate(val = paste0(fmt_pct_ci(sensitivity, sens_lo, sens_hi), "\n", fmt_pct_ci(ppv, ppv_lo, ppv_hi)),
         frac = paste0("top", round(100 * frac))) |>
  select(outcome, sex, frac, val) |>
  pivot_wider(names_from = frac, values_from = val)
e8_body <- e8_perf |>
  left_join(e8_e4, by = c("outcome", "sex")) |>
  arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")), sex) |>
  transmute(Outcome = OUTCOME_LABELS[DOMAIN_OF(outcome)], Sex = sex,
            Events = sprintf("%s/%s (%.1f)", jama_int(events), jama_int(n), 100 * events / n),
            AUC = auc, Slope = slope, CITL = citl, top10, top20, top30) |>
  mutate(across(everything(), fix_minus))
ft_e8 <- flextable(e8_body) |>
  set_header_labels(Outcome = "Outcome", Sex = "Sex",
                    Events = "Events/participants with\nknown outcome, No. (%)",
                    AUC = "AUC (95% CI)", Slope = "Calibration slope\n(95% CI)",
                    CITL = "Calibration-in-the-large\n(95% CI)",
                    top10 = "Top 10%", top20 = "Top 20%", top30 = "Top 30%") |>
  add_header_row(values = c("Outcome", "Sex", "Events/participants with\nknown outcome, No. (%)",
                            "Discrimination and calibration", "Sensitivity and PPV, % (95% CI)"),
                 colwidths = c(1, 1, 1, 3, 3)) |>
  merge_v(j = 1:3, part = "header") |>
  merge_v(j = "Outcome") |> valign(j = 1, valign = "top", part = "body") |>
  align(j = 3:9, align = "center", part = "all") |> align(i = 1, align = "center", part = "header") |>
  hline(i = 2, border = thin_rule) |>
  add_footer_lines(c(
    "Differences between sexes were not tested.",
    "Checkup-based model (final model, coefficients fixed) applied to the temporal validation cohort (FY2022 and FY2023 checkups combined). AUC, DeLong 95% CI; calibration slope and calibration-in-the-large, Wald 95% CI.",
    "Sensitivity and PPV (first and second line in each cell): participants in the top 10%, 20%, or 30% of predicted risk among all eligible participants of both sexes were selected as high risk, as at the checkup; sensitivity and PPV were then calculated within each sex among participants with known outcomes. 95% CIs: bootstrap percentile (stratified by fiscal year; threshold recomputed in each sample).",
    "Sex was taken from the checkup record.",
    "Abbreviations: AUC, area under the receiver operating characteristic curve; FY, fiscal year; MCS, mental component summary; PCS, physical component summary; PPV, positive predictive value.",
    OUTCOME_NOTE, time_note("18m")
  )) |>
  fontsize(size = 7.5, part = "all") |> font(fontname = "Arial", part = "all")
save_tables(list(`eTable 8. Performance of the checkup-based model by sex in the temporal validation cohort (≈18 months)` = ft_e8),
            "eTable8", landscape = TRUE, widths = c(0.9, 0.6, 1.2, 1.05, 1.0, 1.1, 1.05, 1.05, 1.05))

# =============================================================================
# eTable 5. 外部検証での再較正 (09_recalibration.R)
# =============================================================================
recal <- read_derived("recal")
e9_coef <- recal$coef |>
  mutate(val = fmt_ci(estimate, lo, hi)) |>
  select(outcome, term, val) |>
  pivot_wider(names_from = term, values_from = val)
e9_perf <- recal$perf |>
  mutate(val = fmt_ci(estimate, lo, hi),
         key = paste(if_else(version == "Original", "orig", "recal"), stat, sep = "_")) |>
  select(outcome, group, key, val) |>
  pivot_wider(names_from = key, values_from = val)
e9_body <- e9_perf |>
  left_join(e9_coef, by = "outcome") |>
  arrange(match(DOMAIN_OF(outcome), c("pcs", "mcs")), match(group, c("All", "Female", "Male"))) |>
  mutate(intercept = if_else(group == "All", intercept, "—"), slope = if_else(group == "All", slope, "—")) |>
  transmute(Outcome = OUTCOME_LABELS[DOMAIN_OF(outcome)],
            Participants = recode(group, All = "All", Female = "Female", Male = "Male"),
            intercept, slope, orig_slope, orig_citl, recal_slope, recal_citl) |>
  mutate(across(everything(), fix_minus))
ft_e9 <- flextable(e9_body) |>
  set_header_labels(Outcome = "Outcome", Participants = "Participants",
                    intercept = "Intercept (a)", slope = "Slope (b)",
                    orig_slope = "Calibration slope", orig_citl = "Calibration-in-the-large",
                    recal_slope = "Calibration slope", recal_citl = "Calibration-in-the-large") |>
  add_header_row(values = c("Outcome", "Participants", "Recalibration coefficients (95% CI)",
                            "Original model (95% CI)", "After recalibration, cross-validated (approximate 95% CI)"),
                 colwidths = c(1, 1, 2, 2, 2)) |>
  merge_v(j = 1:2, part = "header") |>
  merge_v(j = "Outcome") |> valign(j = 1, valign = "top", part = "body") |>
  align(j = 3:8, align = "center", part = "all") |> align(i = 1, align = "center", part = "header") |>
  hline(i = 3, border = thin_rule) |>
  add_footer_lines(c(
    "Logistic recalibration updated the intercept and slope of the final checkup-based model in the temporal validation cohort (FY2022 and FY2023 checkups combined; participants with known outcomes): recalibrated risk = 1/(1 + exp(−[a + b × LP])), where LP is the linear predictor of the original model (eTable 3). Coefficients were estimated in all participants (Wald 95% CIs).",
    "Calibration after recalibration was evaluated by 10-fold cross-validation within the validation cohort (folds stratified by fiscal year and outcome): a and b were estimated in 9 folds and applied to the held-out fold, and calibration was estimated from the pooled held-out predictions, overall and within each sex (a common recalibration fitted in both sexes). These are approximate Wald 95% CIs that treat the pooled predictions as fixed and omit uncertainty from fitting the recalibration and assigning folds. A single recalibration with a positive slope does not change the AUC; the AUC of pooled predictions from different folds was not assessed.",
    "Calibration slope <1 indicates predictions that are too extreme, >1 predictions that are too moderate; calibration-in-the-large >0 indicates that risk was underestimated on average. —, not applicable (coefficients were estimated in all participants).",
    "Abbreviations: AUC, area under the receiver operating characteristic curve; FY, fiscal year; MCS, mental component summary; PCS, physical component summary.",
    OUTCOME_NOTE, time_note("18m")
  )) |>
  fontsize(size = 7.5, part = "all") |> font(fontname = "Arial", part = "all")
save_tables(list(`eTable 5. Logistic recalibration of the checkup-based model in the temporal validation cohort (≈18 months)` = ft_e9),
            "eTable5", landscape = TRUE, widths = c(1.0, 0.9, 1.1, 1.0, 1.05, 1.25, 1.05, 1.25))

# =============================================================================
# eFigure 1. 健診と調査の時期
# =============================================================================
e1 <- read_derived("efig1_timeline")
tl_plot <- e1$timeline |>
  mutate(cohort = factor(cohort, levels = rev(unique(cohort))),
         row_y = 1.5 * as.numeric(cohort) + if_else(cohort == "Development (FY2020)", 0.35, 0),
         # 図中は時点の役割だけを示し、実測の間隔は脚注へ出す。
         lab = recode(survey, pre = "Pre-checkup\nSF-8", "6m" = "Outcome\n≈6 months",
                      "18m" = "Outcome\n≈18 months"),
         survey = factor(survey, levels = c("pre", "6m", "18m")))
exams <- distinct(tl_plot, cohort, row_y, exam_start, exam_end) |>
  mutate(origin = exam_start + (exam_end - exam_start) / 2,
         half_height = if_else(cohort == "Development (FY2020)", 0.19, 0.12))
# 矢印の共通起点を健診期間の内部に置く。期間の中央は図示用の位置で、
# 個人の起算日は各自の健診日。健診前 SF-8 から水平線を引かない。
tl_followup <- filter(tl_plot, survey != "pre") |>
  mutate(origin = exam_start + (exam_end - exam_start) / 2,
         # 開発コホートでは帯から出る 2 本の水平矢印を上下に離す。
         arrow_y = row_y +
           if_else(cohort == "Development (FY2020)", if_else(survey == "6m", 0.04, -0.04), 0),
         label_y = row_y -
           if_else(cohort == "Development (FY2020)", 0.34, 0.19),
         # 「after checkup」は脚注に書く (10 pt では開発の 2 つのラベルが重なるため 2 行にする)
         outcome_label = recode(as.character(survey),
                                "6m" = "Outcome\n≈6 months",
                                "18m" = "Outcome\n≈18 months")) |>
  # 開発の約半年後と約 1 年半後のラベルは 1 年しか離れておらず、10 pt では接するので、1 年半後を右に寄せる
  mutate(label_hjust = if_else(cohort == "Development (FY2020)" & survey == "18m", 0.2, 0.5)) |>
  arrange(cohort, desc(survey_date))
survey_breaks <- sort(unique(tl_plot$survey_date))
# 英語の月名は OS のロケールに依存させない
month_lab <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%Y"))
# 健診の期間は帯の上のラベルに書く
exams <- exams |>
  mutate(exam_label = paste0("Prediction time\n(checkup)\n", month_lab(exam_start), "–", month_lab(exam_end)))
# 横軸には調査の実施月だけを表示する (10 pt では健診の開始・終了月まで並べると重なるため。2026-10-11)。
# 同月内の 1 日差は 1 目盛りにまとめる。
axis_months <- tibble(date = survey_breaks) |>
  mutate(month = format(date, "%Y-%m")) |>
  group_by(month) |>
  summarise(date = min(date), .groups = "drop") |>
  arrange(date)
axis_breaks <- axis_months$date
cohort_labels <- c("Development\n(FY2020)", "Validation\n(FY2022)",
                   "Validation\n(FY2023)", "Validation, ≈6 months\n(FY2023)")

efig1 <- ggplot(tl_plot) +
  geom_vline(xintercept = axis_breaks, colour = "#E9EDF1", linewidth = 0.25) +
  geom_segment(data = tl_followup,
               aes(x = origin, xend = survey_date - 20, y = arrow_y, yend = arrow_y, colour = survey),
               linewidth = 0.45,
               arrow = grid::arrow(length = grid::unit(1.6, "mm"), type = "closed")) +
  geom_rect(data = exams,
            aes(xmin = exam_start, xmax = exam_end, ymin = row_y - half_height,
                ymax = row_y + half_height),
            fill = scales::alpha(JAMA_COLS[3], 0.30), colour = JAMA_COLS[1], linewidth = 0.4) +
  geom_text(data = exams,
            aes(x = exam_start + (exam_end - exam_start) / 2,
                y = row_y + half_height + 0.06, label = exam_label),
            vjust = 0, size = STYLE$efig_text / .pt,
            lineheight = 1.05, fontface = "bold", colour = JAMA_COLS[1], family = STYLE$font) +
  geom_point(data = filter(tl_plot, survey == "pre"),
             aes(survey_date, row_y, colour = survey, shape = survey), size = 2.7) +
  geom_point(data = tl_followup,
             aes(survey_date, arrow_y, colour = survey, shape = survey), size = 2.7) +
  geom_text(data = filter(tl_plot, survey == "pre"),
            aes(survey_date, row_y - 0.19, label = lab),
            vjust = 1, size = STYLE$efig_text / .pt, lineheight = 1.1, colour = "#555D66", family = STYLE$font) +
  geom_text(data = tl_followup,
            aes(survey_date, label_y, colour = survey, label = outcome_label, hjust = label_hjust),
            vjust = 1, size = STYLE$efig_text / .pt, lineheight = 1.1, fontface = "bold", family = STYLE$font) +
  scale_colour_manual(values = c(pre = JAMA_COLS[7], "6m" = JAMA_COLS[2], "18m" = JAMA_COLS[4])) +
  scale_shape_manual(values = c(pre = 16, "6m" = 17, "18m" = 18)) +
  # 調査の実施月だけを表示する (健診の期間は帯の上のラベル)。目盛りの間は 1 年以上あるのでラベルは水平に置く
  scale_x_date(breaks = axis_breaks, labels = month_lab(axis_breaks),
               expand = expansion(add = c(230, 230))) +   # 両端のラベルが切れないように
  scale_y_continuous(breaks = c(1.5, 3, 4.5, 6.35), labels = rev(cohort_labels),
                     limits = c(0.30, 7.25), expand = expansion(mult = 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = STYLE$efig_text, base_family = STYLE$font) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(colour = "#252B32", size = STYLE$efig_text, lineheight = 1.1, hjust = 1,
                                   margin = margin(r = 8)),
        axis.text.x = element_text(colour = "#333A42", size = STYLE$efig_text, margin = margin(t = 3)),
        axis.ticks.x = element_line(colour = "#89919A", linewidth = 0.3),
        axis.ticks.length = grid::unit(1.2, "mm"),
        axis.line.x = element_line(colour = "#A9B0B8", linewidth = 0.3),
        legend.position = "none",
        plot.margin = margin(5, 5, 4, 4))
# ragg は Windows の非 ASCII のパスで保存に失敗する場合があるため、
# 一時ファイルへ描画してから目的のフォルダへコピーする。
EFIG1_H <- 160
efig1_png_tmp <- tempfile("eFigure1_", fileext = ".png")
ggsave(efig1_png_tmp, efig1, width = STYLE$efig_width, height = EFIG1_H,
       units = "mm", dpi = STYLE$dpi, bg = "white")
stopifnot(file.exists(efig1_png_tmp), file.info(efig1_png_tmp)$size > 0,
          file.copy(efig1_png_tmp, file.path(DIR_FIGURES, "eFigure1.png"), overwrite = TRUE))
unlink(efig1_png_tmp)
ggsave(file.path(DIR_FIGURES, "eFigure1.pdf"), efig1,
       width = STYLE$efig_width, height = EFIG1_H, units = "mm", device = cairo_pdf)
write_emf("eFigure1", STYLE$efig_width, EFIG1_H, \() print(efig1))

# 脚注は画像・PDF に埋め込まず、本文の figure legend 用に別ファイルへ保存する。
efig1_intervals <- e1$timeline |>
  mutate(text = sprintf("%s: %s, %.1f–%.1f years %s the checkup (median %.1f).",
                        cohort, recode(survey, pre = "pre-checkup SF-8", "6m" = "approximately 6-month outcome",
                                       "18m" = "approximately 18-month outcome"),
                        min, max, if_else(survey == "pre", "before", "after"), median))
writeLines(c(
  "eFigure 1. Timing of prediction and outcome assessment.",
  "Prediction is made at each participant's checkup. Light blue bars span the observed checkup dates within each cohort; each participant has a single prediction time at their own checkup. Gray circles indicate the pre-checkup SF-8 assessment. Triangles and diamonds indicate outcome assessment at approximately 6 and 18 months after the checkup, respectively. The horizontal axis shows the months of the displayed surveys on a linear calendar scale; the first and last months of each checkup period are given above its bar.",
  "Horizontal arrows extend from the checkup bar toward the outcome assessments. Arrow colors match the corresponding outcome symbols: orange for approximately 6 months and red for approximately 18 months. The two development-cohort arrows are separated vertically for clarity. Follow-up is measured from each participant's own checkup date, not from the pre-checkup SF-8 assessment, the end of the cohort's checkup period, or an earlier outcome assessment.",
  "The 6- and 18-month labels denote approximate prediction horizons. Actual minimum-to-maximum intervals and medians are listed below, in years rounded to one decimal place. Intervals are calculated using March 31 of the survey year; outcome events are assessed through that date.",
  efig1_intervals$text,
  "The FY2023 validation cohort and the FY2023 approximately 6-month validation cohort have different eligibility criteria and are shown separately.",
  "FY indicates fiscal year; SF-8, 8-item Short-Form Health Survey."
), file.path(DIR_FIGURES, "eFigure1_footnote.txt"), useBytes = TRUE)

# =============================================================================
# eFigure 2. 対象者のフロー
# =============================================================================
# 年度ごとに consort で 対象者全体 → 組入基準を満たさない → 組入基準を満たす → 除外 → 解析コホート の図を作り
# (build_flow() は R/flow_helpers.R)、a–c として上下に並べる。図の中にタイトル・脚注は入れない (説明は Legend に書く)
efig2_g <- map(flow$diagrams, \(f) build_flow(f, fontsize = 8, wrap_width = 50, gap = 0.8))
efig2_geo <- map(efig2_g, flow_geometry)
EFIG2_LETTER_MM <- 5   # 各段の上に置く a–c の文字の高さ
# 1 列に並べると縦に長すぎるので、左の列に a (FY2020)・b (FY2022)、右の列に c (FY2023) を置く
efig2_pos <- local({
  w <- map_dbl(efig2_geo, "width_mm"); h <- map_dbl(efig2_geo, "height_mm") + EFIG2_LETTER_MM
  tibble(x = c(0, 0, max(w[1:2]) + 4), y = c(0, h[1], 0))   # 左上の位置 (mm)。左右の列の間は 4 mm
})
efig2_w <- max(efig2_pos$x + map_dbl(efig2_geo, "width_mm"))
efig2_h <- max(efig2_pos$y + map_dbl(efig2_geo, "height_mm") + EFIG2_LETTER_MM)
draw_efig2 <- function() {
  grid.newpage()
  for (i in seq_along(efig2_g)) {
    geo <- efig2_geo[[i]]; x0 <- efig2_pos$x[i]; y0 <- efig2_pos$y[i]
    grid.text(LETTERS[i], x = unit(x0 + 1, "mm"), y = unit(efig2_h - y0 - 1, "mm"), just = c("left", "top"),
              gp = gpar(fontsize = 9, fontface = "bold", fontfamily = "sans"))
    # draw_flow() と同じ置き方で、この段の範囲の中に描く
    pushViewport(viewport(x = unit(x0, "mm"), y = unit(efig2_h - y0 - EFIG2_LETTER_MM, "mm"), just = c("left", "top"),
                          width = unit(geo$width_mm, "mm"), height = unit(geo$height_mm, "mm")))
    pushViewport(viewport(y = unit(1, "npc") + unit(geo$vp_shift, "mm"), just = "top",
                          width = unit(geo$vp_w, "mm"), height = unit(geo$vp_h, "mm")))
    grid.draw(build_grid(efig2_g[[i]]))
    popViewport(2)
  }
}
# ragg は Windows の非 ASCII のパスで保存に失敗する場合があるため、一時ファイルに描いてからコピーする
save_efig2 <- function(ext, open_dev) {
  tmp <- tempfile("eFigure2_", fileext = ext)
  open_dev(tmp); draw_efig2(); dev.off()
  stopifnot(file.copy(tmp, file.path(DIR_FIGURES, paste0("eFigure2", ext)), overwrite = TRUE))
  unlink(tmp)
}
save_efig2(".png",  \(f) ragg::agg_png(f, width = efig2_w, height = efig2_h, units = "mm", res = STYLE$dpi, background = "white"))
save_efig2(".tiff", \(f) ragg::agg_tiff(f, width = efig2_w, height = efig2_h, units = "mm", res = 600,
                                        compression = "lzw", background = "white"))
save_efig2(".pdf",  \(f) grDevices::cairo_pdf(f, width = efig2_w / 25.4, height = efig2_h / 25.4))
message(sprintf("図: eFigure2 (%.0f x %.0f mm)", efig2_w, efig2_h))
# ↑ 3 つを 1 枚にまとめた図 (8 pt) は、結果レポート (reports/client_report) 用。
# Supplement 用は、JAMA の規定 (図の中の文字は 10 pt) に合わせて 10 pt で作る。10 pt では 3 つが 1 ページに
# 収まらない (縦でも横でも) ので、a–c を別のファイルにして 1 ページずつ貼る (eFigure2a/b/c)
efig2p_g <- map(flow$diagrams, \(f) build_flow(f, fontsize = STYLE$efig_text, wrap_width = 60, gap = 0.8,
                                               fontfamily = STYLE$font))
efig2p_geo <- map(efig2p_g, flow_geometry)
stopifnot(map_dbl(efig2p_geo, "width_mm") <= STYLE$efig_width)
EFIG2P_LETTER_MM <- 6
for (i in seq_along(efig2p_g)) {
  geo <- efig2p_geo[[i]]
  h <- geo$height_mm + EFIG2P_LETTER_MM
  draw_panel <- function() {
    grid.newpage()
    grid.text(LETTERS[i], x = unit(1, "mm"), y = unit(h - 1, "mm"), just = c("left", "top"),
              gp = gpar(fontsize = STYLE$efig_text, fontface = "bold", fontfamily = STYLE$font))
    pushViewport(viewport(x = unit(0, "mm"), y = unit(geo$height_mm, "mm"), just = c("left", "top"),
                          width = unit(geo$width_mm, "mm"), height = unit(geo$height_mm, "mm")))
    pushViewport(viewport(y = unit(1, "npc") + unit(geo$vp_shift, "mm"), just = "top",
                          width = unit(geo$vp_w, "mm"), height = unit(geo$vp_h, "mm")))
    grid.draw(build_grid(efig2p_g[[i]]))
    popViewport(2)
  }
  name <- paste0("eFigure2", letters[i])
  # 幅は他の eFigure と同じ efig_width にそろえる (Word で 3 ページとも同じ倍率になる)
  for (ext in c(".png", ".pdf")) {
    tmp <- tempfile(name, fileext = ext)
    if (ext == ".png") ragg::agg_png(tmp, width = STYLE$efig_width, height = h, units = "mm", res = STYLE$dpi,
                                     background = "white")
    else grDevices::cairo_pdf(tmp, width = STYLE$efig_width / 25.4, height = h / 25.4)
    draw_panel(); dev.off()
    stopifnot(file.copy(tmp, file.path(DIR_FIGURES, paste0(name, ext)), overwrite = TRUE)); unlink(tmp)
  }
  write_emf(name, STYLE$efig_width, h, draw_panel)
  message(sprintf("図: %s (%.0f x %.0f mm)", name, STYLE$efig_width, h))
}

# =============================================================================
# eFigure 3. 感度分析
# =============================================================================
# S4 の行には評価した人数 (結果確定者) を添える (2026-10-10。eMethods の本文から移した)
s4_n <- perf_ext$perf |>
  filter(str_starts(set, "S4_"), outcome == "y_pcs_18m", model == "checkup", stat == "auc") |>
  transmute(fy = str_remove(set, "S4_"), n)
e3 <- sens$efig3 |>
  mutate(label = str_replace(label, "PCS < ", "PCS <"),   # 他の図表の "<40" と表記をそろえる
         fy = str_extract(label, "FY\\d{4}")) |> left_join(s4_n, by = "fy") |>
  mutate(label = if_else(analysis == "S4", sprintf("%s (n = %s)", label, jama_int(n)), label)) |>
  # 行の名前は 10 pt で収まるよう 26 文字で折り返す (eFigure の文字は 10 pt)
  mutate(row_lab = paste0(str_wrap(label, 26), "\n[", type, "]"), row = factor(row_lab, levels = rev(row_lab)))
# AUC と ΔAUC は横軸の範囲が違うので、別々の図を作って patchwork で左右に並べる。
# AUC の軸は他の AUC の図と同じ 0.5–1.0 に固定し、参照線 0.7・0.8 を引く (§7.13 (d))
#   左右とも「タイトル = モデル」「横軸 = 指標」の同じ形にそろえる。
#   favor: ΔAUC の 0 の左右に「どちらのモデルが良いか」を示すラベル (c(左, 右))。
#   目盛りの数字と軸の名前の間に置く。場所を空けるため、軸の名前の上の余白を左右の図で同じだけ広げる
forest_panel <- function(d, title, xlab, xlim, refs, show_y = TRUE, favor = NULL) {
  p <- ggplot(d, aes(est, row, shape = type)) +
    geom_vline(xintercept = refs, colour = "grey75", linewidth = 0.3) +
    geom_hline(yintercept = nrow(d) - 0.5, colour = "grey50", linewidth = 0.3) +   # primary の下に区切り線
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.2, orientation = "y",
                  colour = STYLE$model_colors[["checkup"]]) +     # 健診モデルの色は全図で共通
    geom_point(size = 1.8, colour = STYLE$model_colors[["checkup"]]) +
    scale_shape_manual(values = c("Optimism-corrected" = 16, "Apparent" = 1, "External (temporal)" = 17,
                                  "Later-year performance" = 2)) +
    # 縦軸の範囲を固定する (枠の外の注記で範囲が広がり、左右の行の位置がずれるのを防ぐ)。
    # expand = FALSE にするので、横軸の余白 (範囲の 5%) は自分で足す
    coord_cartesian(xlim = xlim + c(-0.05, 0.05) * diff(xlim), ylim = c(0.4, nrow(d) + 0.6),
                    expand = FALSE, clip = "off") +
    labs(x = xlab, y = NULL, title = title) + theme_efig() +
    theme(axis.title.x = element_text(margin = margin(t = 19, unit = "mm")),   # favor の 3 行分を空ける
          axis.text.y = if (show_y) element_text() else element_blank())
  if (!is.null(favor)) {
    # 下端 (y = 0.4) より下、目盛りの数字の下に置く (clip = "off" で枠の外に描く)
    span <- diff(xlim)
    p <- p + annotate("text", x = refs + c(-0.02, 0.02) * span, y = -0.25, label = favor,
                      hjust = c(1, 0), vjust = 1, lineheight = 0.9,
                      size = STYLE$efig_text / .pt, family = STYLE$font)
  }
  p
}
p_auc <- forest_panel(e3 |> transmute(row, type, est = auc, lo = auc_lo, hi = auc_hi),
                      "Checkup-based model", "AUC (95% CI)", STYLE$auc_limits, STYLE$auc_refs)
d_rng <- range(c(e3$d_e5_lo, e3$d_e5_hi, 0), na.rm = TRUE)
p_delta <- forest_panel(e3 |> transmute(row, type, est = d_e5, lo = d_e5_lo, hi = d_e5_hi),
                        "HRQOL-based −\ncheckup-based model", "ΔAUC (95% CI)",
                        d_rng + c(-0.01, 0.01), 0, show_y = FALSE,
                        favor = c("Favors\ncheckup-\nbased model", "Favors\nHRQOL-\nbased model"))
efig3 <- p_auc + p_delta + plot_layout(guides = "collect") &
  theme(legend.position = "bottom") & guides(shape = guide_legend(nrow = 2, byrow = TRUE))
save_efig(efig3, "eFigure3", height_mm = 180)   # 行の名前が 3 行になる行があるので、行の間を広く取る
# eFigure 3 に描いた値 (結果レポートが本文の数値を照合する。書式は図の元の推定値を小数 3 桁)
readr::write_excel_csv(e3 |> transmute(analysis, label, type,
                                      auc = sprintf("%.3f", auc), auc_lo = sprintf("%.3f", auc_lo), auc_hi = sprintf("%.3f", auc_hi),
                                      d_e5 = sprintf("%.3f", d_e5), d_e5_lo = sprintf("%.3f", d_e5_lo), d_e5_hi = sprintf("%.3f", d_e5_hi)),
                        file.path(DIR_FIGURES, "eFigure3_values.csv"))

# =============================================================================
# eFigure 4. 構成要素ごとの較正曲線 (E9、外部検証、健診ベースモデル)
# =============================================================================
# 換算前 (複合アウトカムの予測リスクをそのまま) を破線、Gondrie らの方法で換算した後を実線で重ねる
EFIG4_ROWS <- c(low_hrqol = "(a) Poor HRQOL (<40), all participants\nwith a known composite outcome",
                ltc3_death = "(b) Care-need level ≥3 or death")
EFIG4_VERSIONS <- c(adapted = "Converted (component-specific risk)",
                    unadjusted = "Unadjusted (risk of the composite outcome)")
efig4_d <- perf_ext$comp_calib |>
  filter(model == "checkup") |>
  mutate(col = factor(paste0("Model for the ", OUTCOME_LABELS[DOMAIN_OF(outcome)]),
                      levels = paste0("Model for the ", OUTCOME_LABELS)),
         row = factor(EFIG4_ROWS[component], levels = EFIG4_ROWS),
         version = factor(EFIG4_VERSIONS[version], levels = EFIG4_VERSIONS))
efig4 <- ggplot(efig4_d, aes(p, observed, linetype = version)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(colour = STYLE$model_colors[["checkup"]], linewidth = 0.7, na.rm = TRUE) +
  facet_grid(row ~ col) +
  scale_linetype_manual(values = setNames(c("solid", "dashed"), EFIG4_VERSIONS)) +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, STYLE$calib_limit), ylim = c(0, STYLE$calib_limit)) +
  labs(x = "Predicted risk", y = "Observed proportion") + theme_efig() +
  theme(strip.text.y = element_text(size = STYLE$efig_text))
save_efig(efig4, "eFigure4", height_mm = 160)
k_lab <- c(low_hrqol = "poor HRQOL", ltc3_death = "care-need level ≥3 or death")
writeLines(c(
  "eFigure 4. Calibration of the checkup-based model for each component of the composite outcome in the temporal validation cohort (≈18 months).",
  "Outcomes at approximately 18 months after the checkup. Dashed lines use the predicted risk of the composite outcome as it is; solid lines use risks converted to component-specific risks by the method of Gondrie et al (eMethods 13): risk = $1 - (1 - p)^{k}$, where $p$ is the predicted risk of the composite outcome and $k$ is the incidence of the component divided by the incidence of the composite outcome in the development cohort.",
  paste0("Conversion factors: ", paste(sprintf("%s, %s: $k$ = %.2f", OUTCOME_LABELS[DOMAIN_OF(perf_ext$comp_k$outcome)],
                                               k_lab[perf_ext$comp_k$component], perf_ext$comp_k$k), collapse = "; "), "."),
  "Both components are evaluated among all participants with a known composite outcome, because converted risks refer to all participants. For component (a), participants who reached care-need level ≥3 or died without an SF-8 score <40 are nonevents. $k$ was calculated with all participants with a known composite outcome in the development cohort as the denominator, counting each component whenever it occurred.",
  "The gray diagonal line indicates perfect calibration. Curves are loess-smoothed.",
  "PCS composite: PCS score <40, care-need level ≥3, or death; MCS composite: MCS score <40, care-need level ≥3, or death.",
  "HRQOL indicates health-related quality of life; MCS, mental component summary; PCS, physical component summary; SF-8, 8-item Short-Form Health Survey."
), file.path(DIR_FIGURES, "eFigure4_footnote.txt"), useBytes = TRUE)

# =============================================================================
# 内部診断の図 (非公表, output/checks/)
# =============================================================================
p_diag <- ggplot(boot_dist, aes(slope_test)) +
  geom_histogram(bins = 30, fill = "grey60") + geom_vline(xintercept = 1, colour = "red") +
  facet_grid(model ~ outcome) +
  labs(x = "Calibration slope of bootstrap models in the original data", y = "Count") + theme_paper()
ggsave(file.path(DIR_CHECKS, "diag_slope_distribution.png"), p_diag, width = 220, height = 160, units = "mm", dpi = 150)
p_lambda <- ggplot(boot_dist, aes(log10(lambda))) +
  geom_histogram(bins = 30, fill = "grey60") + facet_grid(model ~ outcome, scales = "free_y") +
  labs(x = "log10(selected lambda) in bootstrap samples", y = "Count") + theme_paper()
ggsave(file.path(DIR_CHECKS, "diag_lambda_distribution.png"), p_lambda, width = 220, height = 160, units = "mm", dpi = 150)

message("99 完了")
