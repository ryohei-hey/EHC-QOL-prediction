# =============================================================================
# 01_build_analysis_sets.R  解析データ一式 (single source of truth) を作る
# -----------------------------------------------------------------------------
# 目的 : 開発 (D)・外部検証 (V, V_6m)・後年の性能評価 (S4) の対象者と、
#        各時点の複合アウトカムを 1 つのファイルにまとめる。
#        02 以降の解析はすべてこのファイルから始まる (ここ以外で対象者を作り直さない)。
# 入力 : data/processed/cleaned.rds (00 で作成)
# 出力 : data/processed/analysis_sets.rds (個人単位。Git 管理外)
# 対応 : protocol §4 (開発と検証の対応表)・§5 (対象)・§6.2 (アウトカム)・§7.1 (解析集合)
#        人数とイベント数がプロトコル §7.10 の表と一致することを最後に確かめる
# 時間 : 数秒
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")
cl <- readRDS(file.path(DIR_PROCESSED, "cleaned.rds"))

# ---- 1. 各年度で対象の条件 (§5) を満たす人 ----
# build_eligible() は R/fn_data.R。健診前の SF-8 に使う調査年は、
# FY2023 だけ 2023 年が QGEN10 (SF-8 なし) のため 2022 年で代用する (§4)
E20 <- build_eligible(cl, fy = "2020", pre_yr = 2020)
E22 <- build_eligible(cl, fy = "2022", pre_yr = 2022)
E23 <- build_eligible(cl, fy = "2023", pre_yr = 2022)

# ---- 2. 解析集合を作る (§7.1) ----
# D   : FY2020 で条件を満たす人 (開発)
# V   : D に一度も入っていない人を、条件を初めて満たした年度に割り当てる
#       (FY2022 と FY2023 の両方で満たす人は FY2022。1 人 1 記録)
#       「条件」には健診項目がそろうこと (§5.2) も含む。そのため、FY2020 に健診項目の欠測で
#       D から除かれた人は V に入りうる。FY2022 に健診項目が欠測し FY2023 に初めて条件を
#       満たした人は FY2023 に割り当てる (2026-09-22 決定)
# V_6m: FY2023 で条件を満たし、D に入っていない人 (約半年後の外部検証)
# S4  : 重複者を含む各年度の全員 (後年の性能評価)
D   <- E20$data
V22 <- E22$data |> filter(!id %in% D$id)
V23 <- E23$data |> filter(!id %in% D$id, !id %in% V22$id)
V6m <- E23$data |> filter(!id %in% D$id)

# ---- 3. アウトカムを付ける (§6.2) ----
# add_outcome(データ, SF-8, 調査年, 時点の名前)。判定日は調査年の 3 月 31 日。
# 約半年後 = 健診後 1 回目の 3 月調査、約 1 年半後 = 2 回目 (§4)
sf <- cl$sf
D   <- D   |> add_outcome(sf, 2021, "6m") |> add_outcome(sf, 2022, "18m")
V22 <- V22 |> add_outcome(sf, 2024, "18m")      # FY2022 の約半年後 (2023-03) は QGEN10 のため評価しない
V23 <- V23 |> add_outcome(sf, 2025, "18m")
V   <- bind_rows(V22 |> mutate(val_fy = "FY2022"), V23 |> mutate(val_fy = "FY2023"))
V6m <- V6m |> add_outcome(sf, 2024, "6m")
S4  <- bind_rows(E22$data |> add_outcome(sf, 2024, "18m") |> mutate(val_fy = "FY2022"),
                 E23$data |> add_outcome(sf, 2025, "18m") |> mutate(val_fy = "FY2023"))

# ---- 4. プロトコルの数値との照合 ----
# 人数・イベント数・内訳・結果不明が、プロトコル §7.10 の表と完全に一致するかを確かめる。
# 1 つでもずれると stop() で止まる (定義の実装ミスに早く気づくため)。
summarise_outcome <- function(d, y, type) {
  tibble(n_all = nrow(d), n_analysed = sum(!is.na(d[[y]])), events = sum(d[[y]] %in% 1),
         low = sum(d[[type]] %in% "low"), ltc3 = sum(d[[type]] %in% "ltc3"),
         death = sum(d[[type]] %in% "death"), unknown = sum(is.na(d[[y]])))
}
observed <- bind_rows(
  D_6m_pcs   = summarise_outcome(D,   "y_pcs_6m",  "type_pcs_6m"),
  D_6m_mcs   = summarise_outcome(D,   "y_mcs_6m",  "type_mcs_6m"),
  D_18m_pcs  = summarise_outcome(D,   "y_pcs_18m", "type_pcs_18m"),
  D_18m_mcs  = summarise_outcome(D,   "y_mcs_18m", "type_mcs_18m"),
  V22_18m_pcs = summarise_outcome(V22, "y_pcs_18m", "type_pcs_18m"),
  V23_18m_pcs = summarise_outcome(V23, "y_pcs_18m", "type_pcs_18m"),
  V_18m_pcs  = summarise_outcome(V,   "y_pcs_18m", "type_pcs_18m"),
  V_18m_mcs  = summarise_outcome(V,   "y_mcs_18m", "type_mcs_18m"),
  V6m_6m_pcs = summarise_outcome(V6m, "y_pcs_6m",  "type_pcs_6m"),
  V6m_6m_mcs = summarise_outcome(V6m, "y_mcs_6m",  "type_mcs_6m"),
  S4_22_pcs  = summarise_outcome(filter(S4, val_fy == "FY2022"), "y_pcs_18m", "type_pcs_18m"),
  S4_23_pcs  = summarise_outcome(filter(S4, val_fy == "FY2023"), "y_pcs_18m", "type_pcs_18m"),
  .id = "set"
)
# プロトコル §7.10 の値 (NA は表に記載のない項目)
expected <- tribble(
  ~set,          ~n_all, ~n_analysed, ~events, ~low, ~ltc3, ~death, ~unknown,
  "D_6m_pcs",      1478,  1230,  182, 164, 13,  5, 248,
  "D_6m_mcs",      1478,  1230,   95,  77, 13,  5, 248,
  "D_18m_pcs",     1478,  1155,  233, 169, 37, 27, 323,
  "D_18m_mcs",     1478,  1155,  146,  81, 38, 27, 323,
  "V22_18m_pcs",    968,   717,  158, 104, 26, 28, 251,
  "V23_18m_pcs",    160,   111,   32,  21, 10,  1,  49,
  "V_18m_pcs",     1128,   828,  190, 125, 36, 29, 300,
  "V_18m_mcs",     1128,   828,  113,  48, 36, 29, 300,
  "V6m_6m_pcs",     856,   660,  113, 101,  8,  4, 196,
  "V6m_6m_mcs",     856,   660,   56,  44,  8,  4, 196,
  "S4_22_pcs",       NA,  1397,  322,  NA, NA, NA,  NA,
  "S4_23_pcs",       NA,  1279,  311,  NA, NA, NA,  NA
)
check <- observed |>
  pivot_longer(-set, values_to = "observed") |>
  inner_join(pivot_longer(expected, -set, values_to = "expected"), by = c("set", "name")) |>
  filter(!is.na(expected)) |>
  mutate(match = observed == expected)
if (!all(check$match)) {
  print(filter(check, !match))
  stop("プロトコル §7.10 の人数・イベント数と一致しない項目があります (上の表)")
}
message("プロトコル §7.10 の人数・イベント数と ", nrow(check), " 項目すべて一致しました")

# 開発のフロー (§7.10): 2,176 → 2,157 → 2,111 → 1,492 → 1,479 → 1,478
stopifnot(all(unname(E20$flow) == c(2176, 2157, 2111, 1492, 1479, 1478)))
# D と V・V_6m に重なりがないこと
stopifnot(!any(V$id %in% D$id), !any(V6m$id %in% D$id), !anyDuplicated(V$id))

# ---- フロー図 (eFigure 2) 用の除外フラグ ----
# 条件のフラグ (screen_flags() は R/fn_data.R) に、先の年度のコホートに入っていたかを足す。
# 条件をすべて満たした人が build_eligible() の対象者と同じ人であることを確かめる
screen <- list(
  FY2020 = screen_flags(cl, fy = "2020", pre_yr = 2020),
  FY2022 = screen_flags(cl, fy = "2022", pre_yr = 2022) |> mutate(flag_in_dev = id %in% D$id),
  FY2023 = screen_flags(cl, fy = "2023", pre_yr = 2022) |>
    mutate(flag_in_dev = id %in% D$id, flag_in_v22 = id %in% V22$id)
)
for (fy in names(screen)) {
  ok <- screen[[fy]] |> filter(!flag_age, !flag_sf8, !flag_items, !flag_ltc3, !flag_chk)
  E <- list(FY2020 = E20, FY2022 = E22, FY2023 = E23)[[fy]]
  stopifnot(setequal(ok$id, E$data$id))
}
# フロー図は、予測の時点である健診の受診者 (その年度に質問票か計測・検査の記録がある研究参加者) から始める
# (2026-10-18 変更。以前は健診前の 3 月調査の対象住民から始めていた):
# 質問票の記録がない → 健診時に 75 歳未満 → 健診前の 3 月調査に回答していない → SF-8 が算出できない
# → 健診時の除外基準。計測・検査だけで質問票の記録がない人は年齢が分からないので、質問票の記録を先に数える
attend_screen <- function(fy, pre_yr) {
  s <- screen[[fy]]
  sf_pre <- cl$sf |> filter(yr == pre_yr)
  tibble(id = union(s$id, cl$chk$id[cl$chk$exam_fy == substr(fy, 3, 6)])) |>
    left_join(s, by = "id") |>
    mutate(flag_noqx = !id %in% s$id,
           flag_noresp = !id %in% sf_pre$id,
           # 回答したが SF-8 が算出できない (期限後の回答を含む)。flag_sf8 は未回答も含むので分けておく
           flag_sf8 = flag_sf8 & !flag_noresp)
}
screen_attend <- list(FY2020 = attend_screen("FY2020", 2020), FY2022 = attend_screen("FY2022", 2022),
                      FY2023 = attend_screen("FY2023", 2022))
# 組入基準を満たす人は、build_eligible() の対象者を絞る前の段階 (75 歳以上で SF-8 あり) と同じ人
for (fy in names(screen_attend)) {
  s <- screen_attend[[fy]] |> filter(!flag_noqx, !flag_age, !flag_noresp, !flag_sf8)
  stopifnot(setequal(s$id, screen[[fy]] |> filter(!flag_age, !flag_sf8) |> pull(id)))
}

# ---- 5. 保存 ----
analysis_sets <- list(
  D = D, V = V, V6m = V6m, S4 = S4,
  flow = list(FY2020 = E20$flow, FY2022 = E22$flow, FY2023 = E23$flow,
              V = c(FY2022 = nrow(V22), FY2023 = nrow(V23)), V6m = nrow(V6m)),
  screen = screen, screen_attend = screen_attend,
  ids = list(V22 = V22$id, V23 = V23$id),
  outcome_counts = observed
)
saveRDS(analysis_sets, file.path(DIR_PROCESSED, "analysis_sets.rds"))
message("保存: ", file.path(DIR_PROCESSED, "analysis_sets.rds"))
