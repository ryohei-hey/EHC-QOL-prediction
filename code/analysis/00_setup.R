# =============================================================================
# 00_setup.R  共通の設定 (すべての解析ファイルの先頭で source する)
# -----------------------------------------------------------------------------
# 目的 : パッケージの読み込み、フォルダの場所、乱数種、bootstrap の回数、
#        並列計算のコア数、変数のリストを 1 か所にまとめる。
#        ここを書き換えれば、全ファイルの設定が一度に変わる。
# 使い方: 作業ディレクトリをプロジェクトのルート (protocol/ がある場所) にして
#         source("code/analysis/00_setup.R")
# 対応 : protocol/protocol.md §7.11 (Software)・§10 (解析コードパイプライン)
# =============================================================================

# ---- 1. 作業ディレクトリの確認 ----
# 相対パス (data/raw/... など) で読み書きするため、ルートで実行しているかを最初に確かめる
if (!file.exists("protocol/protocol.md")) {
  stop("作業ディレクトリをプロジェクトのルートにしてください (現在: ", getwd(), ")")
}

# ---- 2. パッケージ ----
# suppressPackageStartupMessages() は、読み込み時のメッセージを表示しないための関数
suppressPackageStartupMessages({
  library(tidyverse)   # データの加工 (dplyr など) と作図 (ggplot2)
  library(glmnet)      # リッジ回帰 (L2 罰則つきロジスティック回帰)
  library(pROC)        # 外部検証での AUC の DeLong 95% CI
  library(parallel)    # bootstrap の並列計算 (R に標準で入っている)
})

# ---- 3. 解析の設定 (ここを変えると全体が変わる) ----
SEED <- 20260921       # 乱数種 (§7.11)。bootstrap の標本や CV の分割はすべてここから決まる

# bootstrap の回数。動作確認では小さく (例: 20)、本番はプロトコルどおり 500 / 1000 にする。
# run_all.R から TEST_MODE <- TRUE を先に設定すると、小さい回数で全体を試せる
if (!exists("TEST_MODE")) TEST_MODE <- FALSE
N_BOOT      <- if (TEST_MODE) 20 else 500    # 内部検証の bootstrap (§7.5)
N_BOOT_E4   <- if (TEST_MODE) 50 else 1000   # 高リスク者の抽出の CI (§7.5 E4)
N_BOOT_IPW  <- if (TEST_MODE) 50 else 1000   # IPW の CI (§7.7 S1)
N_CV_REPEAT <- 5                             # λ の選択: 10-fold CV を 5 回繰り返す (§7.5)
N_FOLD      <- 10

# 並列計算に使うコア数。PC の全コアから 2 つを残す (他の作業が固まらないように)
N_CORES <- max(1, parallel::detectCores() - 2)

# TRUE にすると、保存済みの bootstrap 結果があっても計算し直す
if (!exists("REBUILD_BOOT")) REBUILD_BOOT <- FALSE

# ---- 4. フォルダの場所 ----
DIR_RAW       <- "data/raw"
DIR_PROCESSED <- "data/processed"     # 個人単位のデータ (Git 管理外)
DIR_DERIVED   <- "output/derived"     # 図表のもとになる集計済みデータ
DIR_FIGURES   <- "output/figures"     # 論文用の図
DIR_TABLES    <- "output/tables"      # 論文用の表
DIR_CHECKS    <- "output/checks"      # 点検用 (非公表)
for (d in c(DIR_PROCESSED, DIR_DERIVED, DIR_FIGURES, DIR_TABLES, DIR_CHECKS)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- 5. 変数のリスト (§6.1) ----
# 質問票 (QMCOO) 15 項目
ITEMS <- c("health_stts", "mental_stts", "eat_habit", "chew", "swallow", "bwchange",
           "gaitspeed", "fall", "exercise", "memory", "orient", "smoke",
           "goout", "socnw", "socsppt")

# 健診の計測・検査 (モデルに入れる形。対数変換・eGFR 算出・カテゴリ化の後の名前)
CHECKUP_VARS <- c("bmi", "sbp", "dbp", "hdl", "ldl", "log_tg", "fasting",
                  "log_ast", "log_alt", "log_ggtp", "hba1c", "egfr", "upro", "uglu")

# 生データ (103_cgSD) で欠測がないことを求める列 (§5.2)
CHECKUP_RAW <- c("bmi", "sbp", "dbp", "hdl", "ldl", "tg", "bloodtest", "ast", "alt", "ggtp",
                 "hba1c", "cre1", "upro", "uglu")

# 比べる 4 つのモデルの予測因子 (§7.5)
MODELS <- list(
  checkup         = c("age", "sex", ITEMS, CHECKUP_VARS),              # 健診モデル (primary)
  questionnaire   = c("age", "sex", ITEMS),                            # 質問票モデル (E8)
  direct          = c("age", "sex", "pcs_pre", "mcs_pre"),             # HRQOL モデル (E5)
  checkup_direct  = c("age", "sex", ITEMS, CHECKUP_VARS, "pcs_pre", "mcs_pre")  # 健診 + 健診前 HRQOL (E7)
)

# 解析するアウトカム: 領域 (PCS / MCS) × 時点 (約半年後 6m / 約 1 年半後 18m)
OUTCOMES <- tidyr::expand_grid(domain = c("pcs", "mcs"), time = c("18m", "6m")) |>
  mutate(y = paste0("y_", domain, "_", time))

LOW_QOL_CUTOFF <- 40   # PCS / MCS < 40 を低い HRQOL とする (§6.2)

# ---- 6. 共通関数の読み込み ----
for (f in list.files("code/analysis/R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

# ---- 7. derived の保存と読み込み ----
# 図表のもとになるデータは、必ずこの関数で保存する。
# 保存したときの設定 (日時・bootstrap の回数・乱数種・git のコミット) を属性として付けておくと、
# 後で「どの設定で作った図か」を確かめられる。
save_derived <- function(obj, name) {
  git <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE),
                  error = function(e) NA_character_, warning = function(w) NA_character_)
  attr(obj, "meta") <- list(created = Sys.time(), n_boot = N_BOOT, seed = SEED,
                            test_mode = TEST_MODE, git = git[1])
  path <- file.path(DIR_DERIVED, paste0(name, ".rds"))
  saveRDS(obj, path)
  message("保存: ", path)
  invisible(path)
}
read_derived <- function(name) readRDS(file.path(DIR_DERIVED, paste0(name, ".rds")))
