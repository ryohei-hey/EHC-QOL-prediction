# =============================================================================
# fn_table_layout.R  表の列幅と折り返し (99_RENDER_outputs.R の save_tables が使う)
# -----------------------------------------------------------------------------
# 本文と補遺 (manuscript/*.qmd) は output/tables の docx の表をそのまま埋め込むので、
# 表の見た目はここと save_tables で決まる (2026-10-30)。
# Word は 1 行に入るだけ詰めて折り返すので、列幅を比例して縮めるだけでは
# "Outco me" のように単語の途中で切れ、"31.1 (25.5-" / "36.1)" のように CI の途中で切れる。そこで
#   1. 列幅は、出力のフォントで文字の幅を測って決める (fit_table_widths)
#   2. 数の範囲のハイフンは改行しないハイフン、CI の中の空白は改行しない空白にする (keep_together)。
#      CI を 1 まとまりにしても列が入るときだけ (列ごとに決める)
# =============================================================================

# 改行の位置: 数字の間のハイフンは改行しない (U+2011、docx_no_break_hyphen で Word の
# <w:noBreakHyphen/> にする)。"≈18 months" は 1 まとまりにする (改行しない空白)。
# 文字の前のスラッシュでは改行できるようにする (幅のない空白)
tbl_break_points <- function(x) {
  x <- gsub("(\u2248[0-9]+) (?=months?\\b)", "\\1\u00a0", x, perl = TRUE)
  x <- gsub("(?<=[0-9])-(?=[0-9])", "\u2011", x, perl = TRUE)
  gsub("/(?=[[:alpha:]])", "/\u200b", x, perl = TRUE)
}
# 括弧の中の数値の CI ("(0.74 to 0.80)" など) の空白を改行しない空白にする
tbl_keep_ci <- function(x) {
  m <- gregexpr("\\([-\u2011\u22120-9.,% to]+\\)", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), \(ci) gsub(" ", "\u00a0", ci, fixed = TRUE))
  x
}

# 表の各部分 (header・body) のセルの文字列 (書式は無視)。横に結合したセルと、縦に結合して
# 隠れたセル (結合の 2 行目以降) は NA
tbl_cell_text <- function(ft, part) {
  d <- ft[[part]]$content$data
  if (length(d) == 0) return(matrix(character(0), 0, length(ft$col_keys)))
  txt <- matrix(vapply(d, \(ch) paste0(ch$txt, collapse = ""), ""), nrow = nrow(d))
  txt[ft[[part]]$spans$rows != 1 | ft[[part]]$spans$columns == 0] <- NA
  txt
}
# 各セルが縦に何行分を占めるか (縦に結合したセルは結合した行数、それ以外は 1)
tbl_cell_vspan <- function(ft, part) {
  s <- ft[[part]]$spans$columns
  if (length(s) == 0) return(matrix(numeric(0), 0, length(ft$col_keys)))
  pmax(s, 1)
}

# セルを幅 avail (インチ) に入れたときの行数。Word と同じく、単位 (単語) を 1 行に入るだけ
# 詰めて折り返す。u: セルの段落 (\n で区切った部分) ごとの単位の幅
wrap_lines <- function(u, space, avail) {
  sum(vapply(u, function(w) {
    if (length(w) == 0) return(1)
    n <- 1; cur <- w[1]
    for (x in w[-1]) if (cur + space + x > avail) { n <- n + 1; cur <- x } else cur <- cur + space + x
    n
  }, numeric(1)))
}

# 列幅 (インチ) を、合計が width になるように決める。各列は、改行できない単位 (単語。
# 文字の前のハイフンと tbl_break_points のスラッシュの後では切れる) の最大幅より狭くせず、
# 1 行に入る幅より広くしない。その間では、表の高さ (各行のセルの最大行数の合計) が
# 幅あたりいちばん減る列から順に幅を足す (残りは今の幅に比例して配る)。
# CI を 1 まとまりにすると入らないときは、そうするといちばん幅をとる列から順に
# CI の中で折り返せるようにする (keep_ci = FALSE)。
# それでも入らないときだけ最小幅を比例して縮める (単語の途中で切れる)。
# min_w: 列の最小幅 (インチ、列の名前つき)。読みやすさのために広げたい列に使う
fit_table_widths <- function(ft, width, fontsize, font = "Arial", step = 0.02, min_w = NULL) {
  parts <- c("header", "body")
  txt <- do.call(rbind, lapply(parts, \(p) tbl_cell_text(ft, p)))
  vspan <- do.call(rbind, lapply(parts, \(p) tbl_cell_vspan(ft, p)))
  bold <- unlist(lapply(parts, \(p) rep(p == "header", nrow(ft[[p]]$content$data))))
  # セルの左右の余白 (flextable の padding、pt) とゆとり
  pars <- ft$body$styles$pars
  pad <- vapply(seq_along(ft$col_keys),
                \(j) max(pars$padding.left$data[, j] + pars$padding.right$data[, j]), numeric(1)) / 72 + 0.05   # Word は測った幅より少し広く組むので 0.05 インチのゆとり
  measure <- function(x, b) {
    keep <- !is.na(x) & nzchar(x)
    if (!any(keep)) return(0)
    max(gdtools::strings_sizes(x[keep], fontname = font, fontsize = fontsize, bold = b[keep])$width)
  }
  col_max <- function(f, pattern) {
    vapply(seq_along(ft$col_keys), function(j) {
      x <- f(tbl_break_points(txt[, j]))
      x[is.na(x)] <- ""
      units <- regmatches(x, gregexpr(pattern, x, perl = TRUE))
      max(0, measure(unlist(units), rep(bold, lengths(units))))
    }, numeric(1)) + pad
  }
  na_keep <- \(f) \(x) ifelse(is.na(x), NA, f(x))
  unit <- "[^ \t\n\u200b]+?(-(?=[[:alpha:]])|(?=[ \t\n\u200b])|$)"   # 改行しない空白は区切りにしない
  nat <- col_max(na_keep(tbl_keep_ci), "[^\n]+")
  lo <- setNames(rep(0, length(nat)), ft$col_keys)
  stopifnot(all(names(min_w) %in% ft$col_keys))
  lo[names(min_w)] <- min_w
  lo <- pmin(lo, nat)   # 1 行に入る幅より広くはしない
  keep <- rep(TRUE, length(nat))
  if (sum(nat) <= width) return(list(widths = nat * width / sum(nat), keep_ci = keep))
  mn_ci <- pmax(pmin(col_max(na_keep(tbl_keep_ci), unit), nat), lo)
  mn_no <- pmax(pmin(col_max(identity, unit), nat), lo)
  mn <- mn_ci
  while (sum(mn) >= width && any(keep & mn_ci > mn_no)) {
    j <- which.max(ifelse(keep, mn_ci - mn_no, -Inf))
    keep[j] <- FALSE
    mn[j] <- mn_no[j]
  }
  if (sum(mn) >= width) return(list(widths = width * mn / sum(mn), keep_ci = keep))

  # 各セルの単位の幅 (keep_ci の列は CI を 1 単位にする)
  space <- gdtools::strings_sizes(" ", fontname = font, fontsize = fontsize)$width
  cell_units <- lapply(seq_along(nat), function(j) {
    x <- tbl_break_points(txt[, j])
    if (keep[j]) x <- na_keep(tbl_keep_ci)(x)
    lapply(seq_along(x), function(i) {
      if (is.na(x[i]) || !nzchar(x[i])) return(list())
      lapply(strsplit(x[i], "\n", fixed = TRUE)[[1]], function(seg) {
        u <- regmatches(seg, gregexpr(unit, seg, perl = TRUE))[[1]]
        u <- trimws(u[nzchar(trimws(u))])
        if (length(u) == 0) return(numeric(0))
        gdtools::strings_sizes(u, fontname = font, fontsize = fontsize, bold = bold[i])$width
      })
    })
  })
  # 列 j の幅を mn[j] + k * step にしたときの、各行の行数 (k = 0 から nat[j] に届くまで)
  steps_max <- floor((width - sum(mn)) / step)
  k_max <- pmin(steps_max, ceiling((nat - mn) / step))
  lines <- lapply(seq_along(nat), function(j) {
    vapply(0:k_max[j], function(k) {
      avail <- mn[j] + k * step - pad[j]
      # 縦に結合したセルの行数は、結合した行に均等に割り振る (近似)
      vapply(cell_units[[j]], \(u) if (length(u)) wrap_lines(u, space, avail) else 0, numeric(1)) / vspan[, j]
    }, numeric(nrow(txt)))
  })
  k <- rep(0L, length(nat)); left <- steps_max
  # 行の高さはその行でいちばん行数の多いセルで決まるので、1 列だけ広げても高さが減らないことが多い
  # (CI の列がすべて 1 行に入って初めてその行が低くなる)。そこで各セルの行数の合計も小さな重みで足し、
  # 高さが減らない段階でも行数の減る列から広げる
  height <- function(k) {
    m <- sapply(seq_along(k), \(j) as.matrix(lines[[j]])[, k[j] + 1])
    m <- matrix(m, nrow = nrow(txt))
    sum(apply(m, 1, max)) + 0.05 * sum(m)
  }
  repeat {
    h0 <- height(k); best <- NULL; best_rate <- 0
    for (j in seq_along(k)) for (d in seq_len(min(left, k_max[j] - k[j]))) {
      k2 <- k; k2[j] <- k[j] + d
      gain <- h0 - height(k2)
      if (gain > 0) { if (gain / d > best_rate) { best_rate <- gain / d; best <- k2 }; break }
    }
    if (is.null(best)) break
    left <- left - sum(best - k); k <- best
  }
  w <- mn + k * step
  list(widths = w + (width - sum(w)) * w / sum(w), keep_ci = keep)
}

# 見出しと本体の文字列に改行の位置を入れる (keep_ci の列は CI も 1 まとまりにする)。
# 書式 (上付きなど) はチャンクごとに残る
keep_together <- function(ft, keep_ci) {
  for (p in c("header", "body")) {
    d <- ft[[p]]$content$data
    for (i in seq_len(nrow(d))) for (j in seq_len(ncol(d))) {
      f <- if (keep_ci[j]) \(x) tbl_keep_ci(tbl_break_points(x)) else tbl_break_points
      d[[i, j]]$txt <- f(d[[i, j]]$txt)
    }
    ft[[p]]$content$data <- d
  }
  ft
}

# 見出しと本体の最初の行、本体の最後の行と脚注を同じページにまとめる (Word の「次の段落と分離しない」)。
# flextable::paginate(init = FALSE) と違い、表ごとに設定した行のまとまり (paginate(group =)) を消さない
keep_hdr_ftr <- function(ft) {
  nb <- nrow_part(ft, "body"); nf <- nrow_part(ft, "footer")
  if (nrow_part(ft, "header")) ft <- keep_with_next(ft, value = TRUE, part = "header")
  if (nf > 0 && nb > 0) ft <- keep_with_next(ft, i = nb, value = TRUE, part = "body")
  if (nf > 1) ft <- keep_with_next(ft, i = seq_len(nf - 1), value = TRUE, part = "footer")
  # 1 列目だけに文字がある行 (変数名だけの行、"Sex" の下に "Female" などが続く) も次の行とまとめる
  txt <- tbl_cell_text(ft, "body")
  lab <- which(rowSums(!is.na(txt[, -1, drop = FALSE]) & nzchar(txt[, -1, drop = FALSE])) == 0 & seq_len(nb) < nb)
  if (length(lab)) ft <- keep_with_next(ft, i = lab, value = TRUE, part = "body")
  ft
}

# 保存した docx の U+2011 を Word の改行しないハイフン (<w:noBreakHyphen/>) にする
# (Arial に U+2011 の字形がないと、Word が別のフォントで表示するため)
docx_no_break_hyphen <- function(path) {
  path <- normalizePath(path)   # zip::zip(root =) は root に移動して書くので絶対パスにする
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = tmp)
  f <- file.path(tmp, "word", "document.xml")
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  x <- gsub("\u2011", '</w:t><w:noBreakHyphen/><w:t xml:space="preserve">', x, fixed = TRUE)
  writeLines(enc2utf8(x), f, useBytes = TRUE)
  unlink(path)
  zip::zip(path, files = list.files(tmp, recursive = TRUE, all.files = TRUE), root = tmp)
}
