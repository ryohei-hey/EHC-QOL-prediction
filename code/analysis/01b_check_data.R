# =============================================================================
# 01b_check_data.R  解析用データと各変数の分布の点検 (解析前)
# -----------------------------------------------------------------------------
# 目的 : モデルの推定に進む前に、解析用データが正しく作れているかを目で確かめる。
#        summarytools で変数ごとの分布を HTML に出し、summarytools では見られない点
#        (あり得ない値・日付の順序・返送状況・相関) を自動チェックする。
#        値の除外や修正はしない。問題が見つかったら 00・01 に戻って直す。
# 入力 : data/processed/analysis_sets.rds (01 で作成), data/processed/cleaned.rds
# 出力 : output/checks/ (点検用・非公表。Git 管理外)
#        01_dfSummary_D.html / _V.html / _V6m.html  変数ごとの分布の一覧
#        02_descr_by_cohort.html   連続変数の要約 (コホート別)
#        03_categories_by_cohort.html  カテゴリ変数 × コホート
#        04_categories_by_outcome_D.html  カテゴリ変数 × アウトカム (開発)
#        05_sparse_cells_D.csv     アウトカム別に件数の少ないセル (分離の確認)
#        06_out_of_range.csv       あり得ない値の一覧
#        07_date_checks.csv        日付の順序の確認
#        08_return_by_carelevel.html  調査票の返送状況 × 要介護度
#        09_correlation_D.csv      予測因子どうしの相関 (開発)
#        10_return_by_carelevel_members.csv  会員全体での返送状況 × 要介護度 (§4)
# 対応 : protocol §7.13 (c) 内部診断 1–3・6・7
# 時間 : 1 分程度
# 見方 : HTML はブラウザで開く。最後にコンソールに要点が表示される
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(summarytools))

as_sets <- readRDS(file.path(DIR_PROCESSED, "analysis_sets.rds"))
cl      <- readRDS(file.path(DIR_PROCESSED, "cleaned.rds"))

# summarytools の表示設定
# - dfSummary.varnumbers = FALSE : 変数の通し番号を出さない
# - dfSummary.valid.col = FALSE : 「有効な値」の列を省く (欠測の列で十分)
# - footnote = NA : 表の下の注記を省く
st_options(footnote = NA, dfSummary.varnumbers = FALSE, dfSummary.valid.col = FALSE,
           dfSummary.graph.magnif = 0.8, headings = TRUE)

# 点検する変数 (変換前の検査値 raw_ も並べて、対数変換の前後を比べられるようにする)
CONT_VARS <- c("age", "bmi", "sbp", "dbp", "hdl", "ldl",
               "raw_tg", "log_tg", "raw_ast", "log_ast", "raw_alt", "log_alt",
               "raw_ggtp", "log_ggtp", "hba1c", "cre1", "egfr", "pcs_pre", "mcs_pre")
CAT_VARS  <- c("sex", ITEMS, "fasting", "upro", "uglu")

# 変数の日本語ラベル (summarytools の表に表示される)
LABELS <- c(
  age = "年齢 (健診日)", sex = "性別",
  health_stts = "Q1 健康状態", mental_stts = "Q2 生活満足度", eat_habit = "Q3 1日3食",
  chew = "Q4 咀嚼", swallow = "Q5 むせ", bwchange = "Q6 体重減少", gaitspeed = "Q7 歩行速度",
  fall = "Q8 転倒", exercise = "Q9 運動習慣", memory = "Q10 物忘れ", orient = "Q11 年月日",
  smoke = "Q12 喫煙", goout = "Q13 外出", socnw = "Q14 付き合い", socsppt = "Q15 相談相手",
  bmi = "BMI (kg/m2)", sbp = "収縮期血圧 (mmHg)", dbp = "拡張期血圧 (mmHg)",
  hdl = "HDL-C (mg/dL)", ldl = "LDL-C (mg/dL)",
  raw_tg = "中性脂肪 (mg/dL)", log_tg = "log 中性脂肪",
  raw_ast = "AST (U/L)", log_ast = "log AST", raw_alt = "ALT (U/L)", log_alt = "log ALT",
  raw_ggtp = "γ-GTP (U/L)", log_ggtp = "log γ-GTP", hba1c = "HbA1c (%)",
  cre1 = "血清クレアチニン (mg/dL)", egfr = "eGFR (mL/min/1.73m2)",
  fasting = "採血時の食後時間", upro = "尿蛋白", uglu = "尿糖",
  pcs_pre = "健診前 SF-8 PCS", mcs_pre = "健診前 SF-8 MCS",
  t_6m = "健診から約半年後の判定日までの年数", t_18m = "健診から約1年半後の判定日までの年数",
  y_pcs_6m = "PCS 複合 約半年後", y_mcs_6m = "MCS 複合 約半年後",
  y_pcs_18m = "PCS 複合 約1年半後 (primary)", y_mcs_18m = "MCS 複合 約1年半後 (E1)",
  type_pcs_6m = "PCS 複合の内訳 約半年後", type_mcs_6m = "MCS 複合の内訳 約半年後",
  type_pcs_18m = "PCS 複合の内訳 約1年半後", type_mcs_18m = "MCS 複合の内訳 約1年半後"
)
# data frame の各列に label 属性を付ける関数 (summarytools はこれを表示に使う)
add_labels <- function(d) {
  for (v in intersect(names(d), names(LABELS))) attr(d[[v]], "label") <- LABELS[[v]]
  d
}

# 3 つのコホートを 1 つの data frame にまとめる (コホート別の比較に使う)
cohorts <- list(D = as_sets$D, V = as_sets$V, V6m = as_sets$V6m)
out_vars <- function(d) intersect(names(d), c(paste0("t_", c("6m", "18m")),
                                              grep("^(y|type)_", names(d), value = TRUE)))
all_cohorts <- bind_rows(lapply(names(cohorts), \(k)
  cohorts[[k]] |> select(id, all_of(c(CONT_VARS, CAT_VARS))) |> mutate(cohort = k))) |>
  mutate(cohort = factor(cohort, levels = c("D", "V", "V6m")))

chk_path <- function(f) file.path(DIR_CHECKS, f)

# summarytools の表を HTML に保存する関数。
# summarytools は日本語を含むパス ("Dropbox (個人)") に直接書き込めないため、
# 一時フォルダ (英数字だけのパス) に書き出してから output/checks/ にコピーする。
# objs: summarytools の表のリスト (複数の表を 1 つの HTML に続けて書く)
st_html <- function(objs, name, title) {
  tmp <- tempfile(fileext = ".html")
  for (i in seq_along(objs)) {
    # タイトルは最初の表を書くときだけ付ける (追記のときに付けるとエラーになる)
    # method = "browser" で HTML として書き出す (ブラウザは開かない)
    if (i == 1) print(objs[[i]], method = "browser", file = tmp, report.title = title, silent = TRUE)
    else        print(objs[[i]], method = "browser", file = tmp, append = TRUE, silent = TRUE)
  }
  file.copy(tmp, chk_path(name), overwrite = TRUE)
  invisible(chk_path(name))
}

# クロス表 (ctable) を作る関数。
# ctable は呼び出し式から変数名を読み取ろうとするが、all_cohorts[[v]] のような式では
# 失敗して警告が出るため、単純な引数名 (x, y) で呼び、表示名は dnn で与える
ct <- function(x, y, row_name, col_name, prop = "r", useNA = "no") {
  ctable(x, y, prop = prop, useNA = useNA, dnn = c(row_name, col_name))
}

# ---- 1. 変数ごとの分布の一覧 (dfSummary) ----
# 1 変数 1 行で、型・欠測数・分布 (連続は要約統計とヒストグラム、カテゴリは度数と割合) が並ぶ。
# 見るところ: 欠測が 0 か (予測因子)、ヒストグラムの形 (極端な外れ値、対数変換で左右対称に
# 近づいたか)、カテゴリの偏り (ごく少ないカテゴリ)、アウトカムの割合
for (k in names(cohorts)) {
  d <- cohorts[[k]] |>
    select(all_of(c(CONT_VARS, CAT_VARS)), all_of(out_vars(cohorts[[k]]))) |>
    add_labels()
  st_html(list(dfSummary(d, graph.col = TRUE, max.distinct.values = 10)),
          paste0("01_dfSummary_", k, ".html"),
          title = paste0("dfSummary: ", k, " (n = ", nrow(d), ")"))
}

# ---- 2. 連続変数の要約 (コホート別、descr + stby) ----
# 開発 (D) と検証 (V, V6m) で分布がどれだけ違うか (case-mix) を見る。
# skewness (歪度) が大きい (目安 > 1) 変数は右に歪んでいる
descr_by <- stby(data = add_labels(all_cohorts[CONT_VARS]), INDICES = all_cohorts$cohort,
                 FUN = descr,
                 stats = c("n.valid", "mean", "sd", "min", "q1", "med", "q3", "max", "skewness"),
                 transpose = TRUE)
st_html(list(descr_by), "02_descr_by_cohort.html", title = "連続変数の要約 (コホート別)")

# ---- 3. カテゴリ変数 × コホート (ctable) ----
# 列 (コホート) ごとの割合を出す (prop = "c")。開発と検証で回答の分布が違わないかを見る
tabs3 <- lapply(CAT_VARS, \(v)
  ct(all_cohorts[[v]], all_cohorts$cohort, LABELS[[v]], "cohort", prop = "c", useNA = "ifany"))
st_html(tabs3, "03_categories_by_cohort.html", title = "カテゴリ変数 × コホート")

# ---- 4. カテゴリ変数 × アウトカム (開発、ctable) ----
# ロジスティック回帰では、あるカテゴリでイベントが 0 件 (または非イベントが 0 件) だと
# 係数が推定できなくなる (分離)。リッジ回帰は罰則で推定できるが、少ないセルは確認しておく
D <- as_sets$D
tabs4 <- list()
for (y in OUTCOMES$y) {
  for (v in CAT_VARS) {
    tabs4[[length(tabs4) + 1]] <-
      ct(D[[v]], factor(D[[y]], levels = 0:1, labels = c("Non-event", "Event")),
         LABELS[[v]], LABELS[[y]])
  }
}
st_html(tabs4, "04_categories_by_outcome_D.html", title = "カテゴリ変数 × アウトカム (開発)")

# 件数の少ないセルを一覧にする (各カテゴリ × イベントの有無で 5 件未満)
sparse <- map_dfr(OUTCOMES$y, \(y) map_dfr(CAT_VARS, \(v) {
  D |> filter(!is.na(.data[[y]])) |>
    count(level = .data[[v]], event = .data[[y]], .drop = FALSE) |>
    mutate(variable = v, outcome = y, level = as.character(level))
})) |>
  complete(nesting(variable, level, outcome), event = 0:1, fill = list(n = 0)) |>
  filter(n < 5) |>
  arrange(outcome, variable)
write_csv(sparse, chk_path("05_sparse_cells_D.csv"))

# ---- 5. あり得ない値 (範囲外の値) ----
# 臨床的にあり得ない (入力誤りが疑われる) 範囲を変数ごとに決めておき、外れる人を一覧にする。
# ここでは除外しない。一覧を見て、生データを確認するかを判断する
# 2026-09-22 確認: 範囲外 7 件 (SBP 271, DBP 29, HDL 160・157, LDL 318・13, γ-GTP 1184) は
# あり得る値と判断し、そのまま使う
RANGES <- tribble(
  ~variable,  ~lo,  ~hi,
  "age",       75,  110,
  "bmi",       12,   45,
  "sbp",       70,  250,
  "dbp",       30,  150,
  "hdl",       10,  150,
  "ldl",       20,  300,
  "raw_tg",    20, 2000,
  "raw_ast",    5,  500,
  "raw_alt",    2,  500,
  "raw_ggtp",   3, 1000,
  "hba1c",      4,   15,
  "cre1",     0.2,   10,
  "egfr",       5,  150,
  "pcs_pre",    0,   80,
  "mcs_pre",    0,   80
)
out_of_range <- pmap_dfr(RANGES, \(variable, lo, hi) {
  all_cohorts |>
    filter(.data[[variable]] < lo | .data[[variable]] > hi) |>
    transmute(cohort, id, variable = variable, value = .data[[variable]], lo = lo, hi = hi)
})
write_csv(out_of_range, chk_path("06_out_of_range.csv"))

# ---- 6. 日付の順序 ----
# 予測時点 (健診日) より後に起きたことだけがアウトカムになっているかを確かめる。
# CSV は 1 行 1 コホート。t_ で始まる列 (間隔の最小・中央値・最大) 以外の各列は
# 「問題のある人数」で、すべて 0 が期待される
# 2026-09-22 確認: V の 1 人は転出日が健診日より前だったが、判定日までに転出した人として
# 結果不明 (complete case で除外) のまま扱う
date_check <- function(d, k) {
  # 年度は 4/1〜翌年 3/31。翌年度の 4/1 を日付で作って比べる (365 日を足すと、うるう年で 1 日ずれる)
  fy_start <- as.Date(paste0(d$exam_fy, "-04-01"))
  fy_next  <- as.Date(paste0(as.integer(d$exam_fy) + 1L, "-04-01"))
  res <- tibble(
    cohort = k,
    `健診日が年度の外 (4/1〜翌3/31)` = sum(d$exam_date < fy_start | d$exam_date >= fy_next),
    `死亡日が健診日より前`          = sum(d$death_date < d$exam_date, na.rm = TRUE),
    `転出日が健診日より前`          = sum(d$move_date < d$exam_date, na.rm = TRUE),
    `要介護3以上の認定が健診日以前`  = sum(d$ltc3_date <= d$exam_date, na.rm = TRUE)
  )
  # アウトカムの調査の回答日が健診日より前 (健診前の回答をアウトカムに使っていないか)。
  # 回答日は 2024・2025 年の調査だけ記録がある (2020–2022 年は一律 04-01 で、健診より必ず後)
  for (tg in c("6m", "18m")) {
    rcol <- paste0("resp_", tg)
    if (rcol %in% names(d)) {
      res[[paste0("アウトカムの回答日が健診日より前 (", tg, ")")]] <-
        sum(d[[rcol]] < d$exam_date, na.rm = TRUE)
    }
  }
  for (tg in c("6m", "18m")) {
    tcol <- paste0("t_", tg)
    if (tcol %in% names(d)) {
      res[[paste0(tcol, " 最小 (年)")]] <- round(min(d[[tcol]]), 2)
      res[[paste0(tcol, " 中央値 (年)")]] <- round(median(d[[tcol]]), 2)
      res[[paste0(tcol, " 最大 (年)")]] <- round(max(d[[tcol]]), 2)
    }
  }
  res
}
dates <- bind_rows(date_check(as_sets$D, "D"), date_check(as_sets$V, "V"),
                   date_check(as_sets$V6m, "V6m"))
write_csv(dates, chk_path("07_date_checks.csv"))

# ---- 7. 調査票の返送状況 × 調査時点の要介護度 ----
# 要介護 3 以上になると調査票が送付されないため、ほぼ全員が未返送になるはず (§4)。
# 判定日 (3/31) に生存・在住していた人について、要介護度ごとに次の 2 つを比べる。
#   返送         : その年の調査票の記録がある (項目の欠測や期限後の回答も含む)
#   期限内に PCS : 期限 (4/30) 内の回答で PCS が算出できた (アウトカムの判定に使える回答)
care_level_at <- function(ids, date) {
  cl$kd_periods |>
    filter(id %in% ids, kstart <= date, is.na(kend) | kend >= date) |>
    mutate(rank = case_when(klevel %in% c("要介護３", "要介護４", "要介護５") ~ 3L,
                            klevel %in% c("要介護１", "要介護２") ~ 2L,
                            str_starts(klevel, "要支援") ~ 1L)) |>
    group_by(id) |> summarise(rank = max(rank), .groups = "drop")
}
return_status <- function(d, yr, k) {
  date <- as.Date(paste0(yr, "-03-31"))
  alive <- d |> filter(is.na(death_date) | death_date > date, is.na(move_date) | move_date > date)
  sf_yr <- cl$sf |> filter(yr == !!yr)
  alive |>
    select(id) |>
    left_join(care_level_at(alive$id, date), by = "id") |>
    mutate(care_level = factor(coalesce(rank, 0L), levels = 0:3,
                               labels = c("認定なし", "要支援", "要介護1-2", "要介護3-5")),
           returned = factor(id %in% sf_yr$id, levels = c(TRUE, FALSE),
                             labels = c("返送あり", "返送なし")),
           pcs_ok = factor(id %in% sf_yr$id[!is.na(sf_yr$pcs)], levels = c(TRUE, FALSE),
                           labels = c("期限内にPCSあり", "PCSなし")),
           survey = paste0(k, " ", yr, "-03"))
}
ret <- bind_rows(return_status(as_sets$D, 2021, "D"), return_status(as_sets$D, 2022, "D"),
                 return_status(filter(as_sets$V, val_fy == "FY2022"), 2024, "V"),
                 return_status(filter(as_sets$V, val_fy == "FY2023"), 2025, "V"),
                 return_status(as_sets$V6m, 2024, "V6m"))
tabs8 <- list()
for (sv in unique(ret$survey)) {
  r <- filter(ret, survey == sv)
  tabs8[[length(tabs8) + 1]] <- ct(r$care_level, r$returned, paste("要介護度", sv), "調査票の返送")
  tabs8[[length(tabs8) + 1]] <- ct(r$care_level, r$pcs_ok, paste("要介護度", sv), "期限内の PCS")
}
st_html(tabs8, "08_return_by_carelevel.html", title = "調査票の返送状況 × 要介護度")

# 会員全体での返送状況 × 要介護度 (§4 に記載する値)
# 調査票は、1 月の住民台帳から対象年齢の住民を抽出し、要介護の認定者・長期入院者・送付を希望しない人を
# 除いて 3 月に送付される (reference/（修正後）調査対象選定過程資料2025.xlsx)。資料には「要介護対象者」と
# しか書かれていないため、本研究ではこれを要介護 3 以上と解釈した (この集計の要介護度別の未返送率による)。
# 送付の運用を確かめるため、送付時期 (3 月 1 日) に同意済みで生存・在住し、対象年齢
# (その年の 4 月からの年度内に 76 歳以上 = 翌年 3 月 31 日の年齢が 76 歳以上) の会員について、
# その時点の要介護度ごとに、その年の調査票の記録がない割合を集計する。
# (対象年齢未満の会員には調査票が送付されないため、分母から除く。除かないと未返送率が過大になる)
members_return <- map_dfr(c(2020, 2021, 2022, 2024, 2025), \(yr) {
  date <- as.Date(paste0(yr, "-03-01"))
  age_fy_end <- as.numeric(as.Date(paste0(yr + 1, "-03-31")) - cl$members$birth_date) / 365.25
  m <- cl$members |> filter(consent_date < date, age_fy_end >= 76) |> left_join(cl$cens, by = "id") |>
    filter(is.na(death_date) | death_date > date, is.na(move_date) | move_date > date)
  sf_yr <- cl$sf |> filter(yr == !!yr)
  m |>
    left_join(care_level_at(m$id, date), by = "id") |>
    mutate(care_level = factor(coalesce(rank, 0L), levels = 0:3,
                               labels = c("認定なし", "要支援", "要介護1-2", "要介護3-5")),
           returned = id %in% sf_yr$id) |>
    group_by(care_level) |>
    summarise(n = n(), no_return_pct = round(100 * mean(!returned), 1), .groups = "drop") |>
    mutate(survey = yr)
})
write_csv(members_return, chk_path("10_return_by_carelevel_members.csv"))

# ---- 8. 予測因子どうしの相関 (開発) ----
# リッジ回帰は相関の強い予測因子があっても推定できるが、どの組が強く相関するかを把握しておく
# (eTable 3 の標準化係数の解釈に関わる。§7.13 (b) eTable 3 の脚注)
num_pred <- c("age", "bmi", "sbp", "dbp", "hdl", "ldl", "log_tg", "log_ast", "log_alt",
              "log_ggtp", "hba1c", "egfr", "pcs_pre", "mcs_pre")
cor_mat <- cor(D[num_pred], method = "spearman")
cor_long <- as_tibble(cor_mat, rownames = "var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "rho") |>
  filter(var1 < var2) |>
  arrange(desc(abs(rho)))
write_csv(cor_long, chk_path("09_correlation_D.csv"))

# ---- 9. 要点をコンソールに表示 ----
cat("\n==== 点検の要点 ====\n")
cat("出力先:", normalizePath(DIR_CHECKS), "\n\n")
cat("[範囲外の値] ", nrow(out_of_range), "件\n")
if (nrow(out_of_range) > 0) print(count(out_of_range, cohort, variable), n = Inf)
cat("\n[件数 5 未満のセル (開発、カテゴリ × イベント、4 アウトカム合計)] ", nrow(sparse), "セル\n")
if (nrow(sparse) > 0) print(sparse, n = Inf)
cat("\n[日付の確認]\n"); print(as.data.frame(t(dates |> column_to_rownames("cohort"))))
cat("\n[要介護度別: 返送なし / 期限内の PCS なし の割合 (%)]\n")
print(ret |> group_by(survey, care_level) |>
        summarise(n = n(),
                  返送なし_pct = round(100 * mean(returned == "返送なし"), 1),
                  PCSなし_pct  = round(100 * mean(pcs_ok == "PCSなし"), 1),
                  .groups = "drop") |>
        as.data.frame())
cat("\n[相関の強い組 (|rho| >= 0.5)]\n"); print(filter(cor_long, abs(rho) >= 0.5))
cat("\n[会員全体: 要介護度別の返送なしの割合 (%) (§4)]\n")
print(members_return |> select(survey, care_level, no_return_pct) |>
        pivot_wider(names_from = care_level, values_from = no_return_pct) |> as.data.frame())
