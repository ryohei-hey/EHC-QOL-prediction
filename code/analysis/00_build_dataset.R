# =============================================================================
# 00_build_dataset.R  生データ → 解析用に整えたデータ (cleaned.rds)
# -----------------------------------------------------------------------------
# 目的 : data/raw/ の CSV を読み込み、変数を解析に使える形に変換して 1 つにまとめる。
#        生データは変更しない (読むだけ)。
# 入力 : data/raw/104_ceSD.csv (質問票), 103_cgSD.csv (計測・検査),
#        123/126/129/131/132_hq (調査票の SF-8), 102_cnSD.csv (死亡・転出),
#        106_kdSD.csv (介護保険台帳), 101_kaSD.csv (会員情報: 同意日・生年月日)
# 出力 : data/processed/cleaned.rds (個人単位のデータ。Git 管理外)
# 対応 : protocol §4 (データソース)・§6 (変数)・§7.2 (変数導出)
# 時間 : 1 分程度 (CSV の読み込み)
# =============================================================================

source("code/analysis/00_setup.R", encoding = "UTF-8")

# ---- 1. 質問票 (QMCOO) ----
# 回答コードを英語ラベルの factor に直し、健診日の年齢を計算する (関数は R/fn_data.R)
qx <- read_raw("104_ceSD.csv") |> recode_questionnaire()

# ---- 2. 健診の計測・検査 ----
# 対数変換・カテゴリ化を行い、項目がそろっているか (chk_ok) の印を付ける。
# 103_cgSD には chew・exercise など質問票と同じ名前の列もあるが (FY2020 以降は空)、
# recode_checkup() で必要な列だけ取り出すので混ざらない。
chk <- read_raw("103_cgSD.csv") |> recode_checkup()
stopifnot(!anyDuplicated(chk[c("id", "exam_fy", "exam_ymd")]))   # 1 人 1 健診 1 行

# ---- 3. SF-8 (調査票。2020・2021・2022・2024・2025 年) ----
# 2023 年の調査は SF-8 ではなく QGEN10 のため使わない (§4)
sf <- read_sf8_all()
# 回答期限と回答日の状況を表示する (プロトコル §4・§6.2 に記載する数値)
#   期限後の回答     : 回答日が 4/30 より後の件数 (もともと PCS が欠測の回答を含む)
#   うち新たに欠測化 : 期限後の回答のうち PCS があったもの (この処理で欠測にした件数)
#   回答日の記録なし : 2024・2025 年で回答日が NA の件数 (いずれも期限内の回答と確認済みのため採用)
print(sf |>
        group_by(yr) |>
        summarise(返送 = n(), 期限内にPCSあり = sum(!is.na(pcs)),
                  期限後の回答 = sum(late), うち新たに欠測化 = sum(late_scored),
                  回答日の記録なし = if (first(yr) >= 2024) sum(is.na(resp_date)) else NA_integer_,
                  .groups = "drop"))

# ---- 4. 介護保険台帳・住民台帳 ----
kd   <- read_raw("106_kdSD.csv")
ltc  <- make_ltc(kd)                   # 要介護 3 以上・1 以上の最初の認定開始日
cens <- read_raw("102_cnSD.csv") |> make_cens()   # 死亡日・転出日
stopifnot(!anyDuplicated(cens$id))
# 要介護の認定記録で、開始日が欠測のもの (あれば最初の認定日が正しく求まらない可能性がある)
n_kstart_na <- sum(!is.na(kd$klevel) & str_starts(kd$klevel, "要介護") & is.na(as.Date(kd$kstart_ymd)))
message("要介護の認定記録で開始日が欠測の件数: ", n_kstart_na)

# 会員情報 (101_kaSD): 研究への同意日と生年月日。01b で、調査票の送付時期に同意済みで対象年齢の会員の
# 返送状況を要介護度別に集計するために使う (§4)
members <- read_raw("101_kaSD.csv") |>
  transmute(id, consent_date = as.Date(consent_ymd), birth_date = as.Date(birth_ymd))
stopifnot(!anyDuplicated(members$id))

# 点検 (01b: 調査票の返送状況 × 要介護度) のため、台帳の認定期間も残しておく
kd_periods <- kd |>
  filter(!is.na(klevel)) |>
  transmute(id, klevel, kstart = as.Date(kstart_ymd), kend = as.Date(kend_ymd)) |>
  distinct()

# ---- 5. 保存 ----
# list にまとめて 1 つのファイルに保存する。次の 01 はこのファイルだけを読む。
cleaned <- list(qx = qx, chk = chk, sf = sf, ltc = ltc, cens = cens, kd_periods = kd_periods,
                members = members)
saveRDS(cleaned, file.path(DIR_PROCESSED, "cleaned.rds"))
message("保存: ", file.path(DIR_PROCESSED, "cleaned.rds"))
