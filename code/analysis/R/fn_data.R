# =============================================================================
# fn_data.R  データの読み込み・変数の変換・対象の条件・アウトカムの定義
# -----------------------------------------------------------------------------
# 00_build_dataset.R と 01_build_analysis_sets.R から使う関数。
# 定義はプロトコル §4–§6 に従う。code/feasibility_checkup_counts.R から移植し、
# V・V_6m の割り当てだけを変えた (健診項目がそろうことを対象の条件に含める)。
# プロトコルの人数・イベント数との照合は 01_build_analysis_sets.R で行う。
# =============================================================================

# ---- 生データの読み込み ----
# すべての列を文字として読む (ID の先頭の 0 や、コードが数値に変わってしまうのを防ぐ)。
# 数値への変換は、必要な列だけ後で明示的に行う。
read_raw <- function(file) {
  readr::read_csv(file.path(DIR_RAW, file), col_types = readr::cols(.default = "c"),
                  progress = FALSE)
}

# ---- 質問票 (QMCOO, 104_ceSD) の変換 ----
# 回答コードを、英語のラベルをもつ factor (カテゴリ変数) に直す。
# factor の最初の水準が参照カテゴリになる。参照は「最もリスクの低い回答」(§7.2)。
# ラベルは eTable 1 (§7.13 (b')) の英語表記に合わせる。
recode_questionnaire <- function(ce) {
  # 生データのコード: 1 = はい、2 = いいえ。関数名は参照カテゴリ (最初の水準) を表す
  ref_no  <- function(x) factor(x, levels = c("2", "1"), labels = c("No", "Yes"))  # 参照 = No
  ref_yes <- function(x) factor(x, levels = c("1", "2"), labels = c("Yes", "No"))  # 参照 = Yes
  ce |>
    mutate(
      exam_date  = as.Date(exam_ymd),
      birth_date = as.Date(birth_ymd),
      # 年齢 = (健診日 − 生年月日) / 365.25 (§5.1)
      age = as.numeric(exam_date - birth_date) / 365.25,
      sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
      health_stts = factor(health_stts, levels = 1:5,
                           labels = c("Excellent", "Good", "Fair", "Poor", "Very poor")),
      mental_stts = factor(mental_stts, levels = 1:4,
                           labels = c("Satisfied", "Moderately satisfied",
                                      "Moderately dissatisfied", "Dissatisfied")),
      smoke = factor(smoke, levels = c("2", "3", "1"), labels = c("No", "I quit", "Yes")),
      # 「はい」がリスクの低い回答の項目 (3 食食べる・運動する・外出する・付き合い・相談相手)
      across(c(eat_habit, exercise, goout, socnw, socsppt), ref_yes),
      # 「はい」がリスクの高い回答の項目
      across(c(chew, swallow, bwchange, gaitspeed, fall, memory, orient), ref_no)
    ) |>
    select(id, exam_fy, exam_ymd, exam_date, birth_date, age, sex, all_of(ITEMS))
}

# ---- 健診の計測・検査 (103_cgSD) の変換 ----
# 右に歪む検査値 (中性脂肪・AST・ALT・γ-GTP) は自然対数に変換する (§6.1)。
# 変換前の値も点検 (01b) のために残す (raw_ という接頭辞)。
# chk_ok = モデルに使う健診項目がすべてそろっているか (§5.2)。
#   変換した後の値で判定する (数値にできない文字・0 以下の値の対数・想定外のコードは
#   NA や -Inf になるため、変換前だけで判定すると見逃す)
recode_checkup <- function(cg) {
  num_cols <- c("bmi", "sbp", "dbp", "hdl", "ldl", "tg", "ast", "alt", "ggtp", "hba1c", "cre1")
  model_cols <- c("bmi", "sbp", "dbp", "hdl", "ldl", "log_tg", "log_ast", "log_alt", "log_ggtp",
                  "hba1c", "cre1", "fasting", "upro", "uglu")
  cg |>
    select(id, exam_fy, exam_ymd, all_of(CHECKUP_RAW)) |>
    mutate(
      across(all_of(num_cols), \(x) suppressWarnings(as.numeric(x))),
      log_tg = log(tg), log_ast = log(ast), log_alt = log(alt), log_ggtp = log(ggtp),
      # 採血時の食後時間: 10 時間以上を参照 (§6.1)
      fasting = factor(bloodtest, levels = c("2", "3", "4"),
                       labels = c(">=10 h", "3.5 to <10 h", "<3.5 h")),
      # 尿蛋白: − (参照) / ± / + 以上。尿糖: − (参照) / ± 以上
      upro = factor(case_when(upro == "1" ~ "-", upro == "2" ~ "+/-",
                              upro %in% c("3", "4", "5") ~ ">=1+"),
                    levels = c("-", "+/-", ">=1+")),
      uglu = factor(case_when(uglu == "1" ~ "-", uglu %in% c("2", "3", "4", "5") ~ ">=+/-"),
                    levels = c("-", ">=+/-"))
    ) |>
    # 変換後のモデル変数がすべて有限の値 (カテゴリは NA でない) か。クレアチニンは eGFR の計算に使うため正の値も求める
    mutate(chk_ok = if_all(all_of(model_cols), \(x) if (is.numeric(x)) is.finite(x) else !is.na(x)) &
             cre1 > 0) |>
    rename(raw_tg = tg, raw_ast = ast, raw_alt = alt, raw_ggtp = ggtp) |>
    select(-bloodtest)
}

# eGFR (日本腎臓学会の式): 194 × Cr^−1.094 × 年齢^−0.287 (女性は × 0.739) (§6.1)
calc_egfr <- function(cre, age, sex) {
  194 * cre^-1.094 * age^-0.287 * ifelse(sex == "Female", 0.739, 1)
}

# ---- SF-8 (調査票) ----
# 各年の調査票から id・PCS・MCS・回答日を取り出し、縦長 (1 人 1 年 1 行) にまとめる。
# 回答期限: 調査年の 4 月 30 日より後の回答は欠測とする (§6.2)。
# 2020–2022 年は回答日が一律 04-01 と記録されているため、全件を期限内として扱う。
# 2024・2025 年も、紙の回答の約 2 割は回答日が記録されていない (resp_date が NA)。
# これらはいずれも期限内の回答であることを確認済みなので、採用する (§6.2。2026-09-22 確認。件数は 00 で表示する)。
# 返送されたが項目の欠測で PCS・MCS を算出できない人は、行はあるが pcs・mcs が NA になる。
read_sf8_one <- function(file, yr, pcs_col, mcs_col, date_col = NULL) {
  h <- read_raw(file)
  resp <- if (is.null(date_col)) as.Date(NA) else as.Date(h[[date_col]])
  tibble(id = h$id, yr = yr,
         pcs = as.numeric(h[[pcs_col]]), mcs = as.numeric(h[[mcs_col]]),
         resp_date = resp) |>
    mutate(late = !is.na(resp_date) & resp_date > as.Date(paste0(yr, "-04-30")),
           # 期限後の回答で、もともと PCS があったもの (= この処理で新たに欠測にしたもの)
           late_scored = late & !is.na(pcs),
           pcs = if_else(late, NA_real_, pcs),
           mcs = if_else(late, NA_real_, mcs))
}
read_sf8_all <- function() {
  sf <- bind_rows(
    read_sf8_one("123_hq_2020SD.csv", 2020, "s20hq_sf8pcs", "s20hq_sf8mcs"),
    read_sf8_one("126_hq_2021SD.csv", 2021, "s21hq_sf8pcs", "s21hq_sf8mcs"),
    read_sf8_one("129_hq_2022SD.csv", 2022, "s22hq_sf8pcs", "s22hq_sf8mcs"),
    read_sf8_one("131_hq_2024SD+age+sex+edu+sf8+qdis1+qdismcc.csv", 2024, "pcs", "mcs", "exam_ymd"),
    read_sf8_one("132_hq_2025SD+age+sex.csv", 2025, "pcs", "mcs", "exam_ymd")
  )
  # 同じ人が同じ年に 2 回回答していないか (1 人 1 年 1 行であること) を確かめる
  stopifnot(!anyDuplicated(sf[c("id", "yr")]))
  sf
}

# ---- 介護保険台帳 (106_kdSD) ----
# 要介護 3 以上 (要介護 3〜5) と要介護 1 以上の、最初の認定開始日を 1 人 1 行で求める。
# 要介護 3 以上はアウトカムと除外に、要介護 1 以上は感度分析 S2 に使う。
make_ltc <- function(kd) {
  kd |>
    filter(!is.na(klevel)) |>
    mutate(kstart = as.Date(kstart_ymd),
           care3 = klevel %in% c("要介護３", "要介護４", "要介護５"),
           care1 = str_starts(klevel, "要介護")) |>
    group_by(id) |>
    # 認定開始日が欠測の記録があっても、日付のある記録の最小値を使う (na.rm = TRUE)。
    # 日付のある記録が 1 つもなければ NA (00 で件数を確認する)
    summarise(
      ltc3_date = first_date(kstart[care3]),
      ltc1_date = first_date(kstart[care1]),
      .groups = "drop"
    )
}

# 日付の最小値 (NA を除く)。すべて NA または空なら NA を返す
first_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) as.Date(NA) else min(x)
}

# ---- 住民台帳 (102_cnSD): 死亡日・転出日 ----
make_cens <- function(cn) {
  cn |>
    transmute(id,
              death_date = if_else(cens_cd == "死亡", as.Date(cens_ymd), as.Date(NA)),
              move_date  = if_else(cens_cd == "転出", as.Date(cens_ymd), as.Date(NA)))
}

# ---- 対象の条件 (§5) ----
# fy     : 健診の年度 (例 "2020")
# pre_yr : 健診前の SF-8 に使う調査年 (FY2023 は 2023 年が QGEN10 のため 2022 年で代用)
# 戻り値 : list(data = 条件を満たした人のデータ, flow = 各段階の人数)
build_eligible <- function(cl, fy, pre_yr) {
  flow <- c()
  d <- cl$qx |> filter(exam_fy == fy)
  stopifnot(!anyDuplicated(d$id))                      # 1 人 1 記録
  flow["Questionnaire respondents"] <- nrow(d)

  d <- d |> filter(age >= 75)
  flow["Aged >=75 years"] <- nrow(d)

  d <- d |> filter(if_all(all_of(c(ITEMS, "sex")), \(x) !is.na(x)))
  flow["All 15 items answered"] <- nrow(d)

  pre <- cl$sf |> filter(yr == pre_yr) |> select(id, pcs_pre = pcs, mcs_pre = mcs)
  d <- d |> left_join(pre, by = "id") |> filter(!is.na(pcs_pre), !is.na(mcs_pre))
  flow["Pre-checkup SF-8 available"] <- nrow(d)

  # 健診日の時点ですでに要介護 3 以上の人を除く (予測の前にイベントが起きているため)
  d <- d |> left_join(cl$ltc, by = "id") |> filter(is.na(ltc3_date) | ltc3_date > exam_date)
  flow["No care level >=3 at checkup"] <- nrow(d)

  # 同じ健診の受診日の計測・検査の記録と対応づけ、項目がそろう人に限る
  d <- d |>
    left_join(cl$chk, by = c("id", "exam_fy", "exam_ymd")) |>
    filter(chk_ok %in% TRUE) |>
    mutate(egfr = calc_egfr(cre1, age, sex)) |>
    left_join(cl$cens, by = "id")
  flow["Checkup measurements complete"] <- nrow(d)

  list(data = d, flow = flow)
}

# ---- フロー図 (eFigure 2) 用の除外フラグ ----
# build_eligible() と同じ条件を、絞り込まずに 1 人 1 行のフラグ列 (TRUE = 当てはまらない / 除外) で持つ。
# フロー図では組入基準 (§5.1) と除外基準 (§5.2) を分けて数えるため、build_eligible() とは数える順が違う
screen_flags <- function(cl, fy, pre_yr) {
  pre <- cl$sf |> filter(yr == pre_yr) |> select(id, pcs_pre = pcs, mcs_pre = mcs)
  s <- cl$qx |>
    filter(exam_fy == fy) |>
    left_join(pre, by = "id") |>
    left_join(cl$ltc, by = "id") |>
    left_join(cl$chk |> select(id, exam_fy, exam_ymd, chk_ok), by = c("id", "exam_fy", "exam_ymd")) |>
    # filter() が NA の行を落とすのと同じ扱いにするため、条件が NA なら「当てはまらない」とする
    transmute(id,
              flag_age   = !coalesce(age >= 75, FALSE),
              flag_sf8   = is.na(pcs_pre) | is.na(mcs_pre),
              flag_items = !if_all(all_of(c(ITEMS, "sex")), \(x) !is.na(x)),
              flag_ltc3  = !coalesce(is.na(ltc3_date) | ltc3_date > exam_date, FALSE),
              flag_chk   = !(chk_ok %in% TRUE))
  stopifnot(!anyDuplicated(s$id))
  s
}

# ---- 複合アウトカム (§6.2) ----
# yr  : アウトカムの調査年 (判定日はその年の 3 月 31 日)
# tag : 列名に付ける時点の名前 ("6m" = 約半年後、"18m" = 約 1 年半後)
# 作る列 (tag = "18m" の例):
#   pcs_18m, mcs_18m   : その年の SF-8 の値 (欠測は NA)
#   ltc3_18m, death_18m: 判定日までに要介護 3 以上 / 死亡に至ったか
#   ltc1_18m           : 健診日より後、判定日までに初めて要介護 1 以上の認定を受けたか (S2 用。
#                        健診時にすでに要介護 1–2 だった人は「新たな認定」ではないので FALSE)
#   returned_18m       : その年の調査票が返送されたか (PCS が算出できたかとは別。01b の点検用)
#   resp_18m           : 回答日 (2024・2025 年のみ記録あり。01b の点検用)
#   moved_18m          : 判定日までに転出し、転出までにイベントが確定していないか (結果不明)
#   t_18m              : 健診から判定日までの年数
#   y_pcs_18m, y_mcs_18m      : 複合アウトカム (1 = イベント、0 = 非イベント、NA = 結果不明)
#   type_pcs_18m, type_mcs_18m: イベントの内訳 (low > ltc3 > death の順に 1 つに分類)
add_outcome <- function(d, sf, yr, tag) {
  cutoff <- as.Date(paste0(yr, "-03-31"))
  s <- sf |> filter(yr == !!yr) |>
    transmute(id, pcs_v = pcs, mcs_v = mcs, resp_v = resp_date, returned_v = TRUE)
  d <- d |>
    left_join(s, by = "id") |>
    mutate(
      ltc3_ev  = !is.na(ltc3_date)  & ltc3_date  <= cutoff,
      death_ev = !is.na(death_date) & death_date <= cutoff,
      ltc1_ev  = !is.na(ltc1_date)  & ltc1_date > exam_date & ltc1_date <= cutoff,
      returned_v = coalesce(returned_v, FALSE),
      # 転出後の要介護認定・死亡は把握できないため、転出前にイベントが確定していなければ結果不明
      moved    = !is.na(move_date) & move_date <= cutoff &
                 !((ltc3_ev & ltc3_date <= move_date) | (death_ev & death_date <= move_date)),
      t_years  = as.numeric(cutoff - exam_date) / 365.25
    )
  for (dom in c("pcs", "mcs")) {
    v <- d[[paste0(dom, "_v")]]
    d[[paste0("y_", dom, "_", tag)]] <-
      make_composite(v, d$ltc3_ev, d$death_ev, d$moved, LOW_QOL_CUTOFF)
    d[[paste0("type_", dom, "_", tag)]] <-
      case_when(!is.na(v) & v < LOW_QOL_CUTOFF & !d$moved ~ "low",
                d$ltc3_ev & !d$moved ~ "ltc3",
                d$death_ev & !d$moved ~ "death")
  }
  d |> rename("pcs_{tag}" := pcs_v, "mcs_{tag}" := mcs_v,
              "ltc3_{tag}" := ltc3_ev, "death_{tag}" := death_ev, "ltc1_{tag}" := ltc1_ev,
              "moved_{tag}" := moved, "t_{tag}" := t_years,
              "resp_{tag}" := resp_v, "returned_{tag}" := returned_v)
}

# 複合アウトカムの判定 (閾値を変える感度分析 S3 でも使う)
#   イベント  : 低い HRQOL (v < cutoff)、要介護 3 以上、死亡のいずれか
#   非イベント: SF-8 に回答していて v >= cutoff で、要介護 3 以上にも死亡にも至っていない
#   結果不明  : それ以外 (SF-8 欠測かつ台帳でイベントなし、または転出)
make_composite <- function(v, ltc3_ev, death_ev, moved, cutoff) {
  case_when(moved ~ NA_integer_,
            (!is.na(v) & v < cutoff) | ltc3_ev | death_ev ~ 1L,
            !is.na(v) ~ 0L,
            .default = NA_integer_)
}

# ---- 複合アウトカムの構成要素 (E9, §7.5) ----
# d は複合アウトカムの結果が確定した人 (y が NA でない人) に限った解析対象。
# 構成要素ごとに、対象 (in_pop: 論理値) とアウトカム (y: 0/1) を返す。
#   low_hrqol : 判定日までに要介護 3 以上にも死亡にも至らず、SF-8 の値がある人。イベント = 値 < 40
#               (生存し、重い要介護状態に至らず、回答した人に条件づけた評価)
#   ltc3_death: 解析対象の全員。イベント = 判定日までの要介護 3 以上または死亡
# 対象を複合アウトカムの解析対象の中に限るのは、複合アウトカムの AUC と同じ対象の中で分解し、
# 開発では同じ bootstrap 標本で optimism を補正するため
make_components <- function(d, dom, tag) {
  v  <- d[[paste0(dom, "_", tag)]]
  ld <- d[[paste0("ltc3_", tag)]] | d[[paste0("death_", tag)]]
  list(low_hrqol  = list(in_pop = !ld & !is.na(v), y = as.integer(!is.na(v) & v < LOW_QOL_CUTOFF)),
       ltc3_death = list(in_pop = rep(TRUE, nrow(d)), y = as.integer(ld)))
}
