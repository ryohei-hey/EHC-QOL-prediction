# =============================================================================
# run_all.R  解析パイプラインを番号順にすべて実行する
# -----------------------------------------------------------------------------
# 使い方 (作業ディレクトリをプロジェクトのルートにして):
#   source("code/analysis/run_all.R", encoding = "UTF-8")
# または、ターミナルから:
#   Rscript code/analysis/run_all.R
#
# 設定:
#   TEST_MODE    TRUE にすると bootstrap の回数を減らして全体を短時間で試せる (動作確認用)。
#                論文の結果は FALSE (プロトコルどおり 500 / 1000 回) で作る
#   REBUILD_BOOT TRUE にすると、保存済みの bootstrap の結果 (キャッシュ) を使わずに計算し直す
#
# 時間の目安 (20 コア): 本番 2 時間前後 (03 と 05 がほとんど)。TEST_MODE では 5〜10 分。
# 図表の見た目だけを直すときは、99_RENDER_outputs.R だけを実行すればよい。
# =============================================================================

TEST_MODE    <- FALSE
REBUILD_BOOT <- FALSE

scripts <- c(
  "00_build_dataset.R",        # 生データ → cleaned.rds
  "01_build_analysis_sets.R",  # 解析データ一式 (D・V・V_6m・S4)。プロトコルの人数と照合
  "01b_check_data.R",          # 解析前の点検 (output/checks/)
  "02_descriptive.R",          # Table 1, eTable 1, eFigure 1・2 のデータ
  "03_development_ridge.R",    # 開発と bootstrap 内部検証 (重い)
  "04_external_validation.R",  # 時間的外部検証、高リスク者の抽出
  "05_sensitivity.R",          # 感度分析 S1–S3・S5
  "06_missing_comparison.R",   # eTable 2 のデータ
  "07_diagnostics.R",          # 内部診断
  "08_by_sex.R",               # 性別ごとの主たる結果 (eTable 9)
  "09_recalibration.R",        # 外部検証での再較正 (eTable 5)
  "99_RENDER_outputs.R"        # 論文用の図表
)

t_start <- Sys.time()
rscript <- file.path(R.home("bin"), "Rscript")
for (s in scripts) {
  message("\n########## ", s, " ##########")
  t0 <- Sys.time()
  # 各ファイルは別の R プロセスで実行する (前のファイルで作った変数が紛れ込まないように)。
  # 設定 (TEST_MODE・REBUILD_BOOT) を先に代入してから読み込むので、00_setup.R がそれを使う
  cmd <- sprintf("TEST_MODE <- %s; REBUILD_BOOT <- %s; source('code/analysis/%s', encoding = 'UTF-8')",
                 TEST_MODE, REBUILD_BOOT, s)
  status <- system2(rscript, c("-e", shQuote(cmd)))
  if (status != 0) stop(s, " でエラーが起きたため中止しました")
  message("   (", s, ": ", round(difftime(Sys.time(), t0, units = "mins"), 1), " 分)")
}
message("\nすべて完了: ", round(difftime(Sys.time(), t_start, units = "mins"), 1), " 分")
sessionInfo()
