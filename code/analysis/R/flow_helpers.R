# =============================================================================
# flow_helpers.R : 論文用の患者選択フロー図(consort パッケージ)の共通関数
# -----------------------------------------------------------------------------
# 基本の形(Box の名前は研究ごとに変える):
#
#   スクリーニング
#        |----> 組入基準を満たさない(理由ごとの人数)
#   組入基準を満たす
#        |----> 除外(理由ごとの人数)
#   解析コホート
#
# flow_counts()       : 人数を手で入れてフロー図のもとデータを作る
# flow_from_data()    : 除外フラグ列を持つデータフレームから人数を数える(階層的に振り分け)
# check_flow()        : 引き算が合っているか確認する(合わなければ止まる)
# build_flow()        : consort で図を組み立てる(フォント、線、箱の幅を論文用にそろえる)
# save_flow()         : TIFF(600 dpi, LZW)と PDF(ベクター)で実寸保存する
# flow_counts_table() : 図の人数を 1 行 1 箱の表にする(原稿の数値突合に使う)
#
# 必要なパッケージ: consort, grid, ragg
# 前提: PATHS$figures(保存先)を 00_setup.R などで決めておく
# =============================================================================

library(consort)
library(grid)


# ---- 1. もとデータを作る ----

# 人数を手で入れる場合。
#   boxes      : 縦に並ぶ箱。list(label = "Patients screened", n = 12345) を上から順に
#   exclusions : 箱と箱の間の横の箱。length(boxes) - 1 個。
#                list(label = "Excluded", reasons = c("ICU stay <24 h" = 300, ...))
#                理由を出さないときは list(label = "Excluded", n = 500)
#                除外がない段は NULL
flow_counts <- function(boxes, exclusions) {
  stopifnot(length(exclusions) == length(boxes) - 1)
  flow <- list(boxes = boxes, exclusions = exclusions)
  for (i in seq_along(flow$exclusions)) {
    ex <- flow$exclusions[[i]]
    if (is.null(ex)) next
    if (is.null(ex$n)) flow$exclusions[[i]]$n <- sum(ex$reasons)
  }
  check_flow(flow)
}

# データフレームから数える場合。
#   df       : スクリーニング対象の全員(1 行 1 人)
#   labels   : 縦の箱の名前(上から順)
#   steps    : 段ごとの除外。list(list(label = "...", reasons = c("表示名" = "フラグ列名", ...)), ...)
#              フラグ列は TRUE = その理由に当たる(除外する)。NA は FALSE として扱う
# 1 人が複数の理由に当たるときは、reasons に書いた順で最初の理由に数える(階層的に振り分け)。
# こうすると理由ごとの人数の合計が除外人数と一致する。重なりの人数は message で報告する。
flow_from_data <- function(df, labels, steps) {
  stopifnot(length(steps) == length(labels) - 1)
  boxes <- list(list(label = labels[1], n = nrow(df)))
  exclusions <- vector("list", length(steps))
  remain <- rep(TRUE, nrow(df))

  for (i in seq_along(steps)) {
    st <- steps[[i]]
    counts <- integer(0)
    stage_start <- remain
    for (nm in names(st$reasons)) {
      col <- st$reasons[[nm]]
      if (!col %in% names(df)) stop("列がありません: ", col)
      hit <- remain & (df[[col]] %in% TRUE)
      n_any <- sum(stage_start & (df[[col]] %in% TRUE))   # この段の先頭で残っていて、この理由に当たる人
      counts[nm] <- sum(hit)
      if (n_any > sum(hit)) {
        message(sprintf("[flow] %s: %d 人は先の理由で数えたので、ここでは %d 人", nm, n_any - sum(hit), sum(hit)))
      }
      remain <- remain & !hit
    }
    exclusions[[i]] <- list(label = st$label, reasons = counts, n = sum(counts))
    boxes[[i + 1]] <- list(label = labels[i + 1], n = sum(remain))
  }
  flow <- check_flow(list(boxes = boxes, exclusions = exclusions))
  attr(flow, "included") <- remain   # 解析コホートに残った人(論理ベクトル)
  flow
}

# 引き算の確認。上の箱 - 除外 = 下の箱、理由の合計 = 除外人数
check_flow <- function(flow) {
  b <- flow$boxes
  for (i in seq_along(flow$exclusions)) {
    ex <- flow$exclusions[[i]]
    n_ex <- if (is.null(ex)) 0 else ex$n
    if (b[[i]]$n - n_ex != b[[i + 1]]$n) {
      stop(sprintf("人数が合いません: %s (%d) - %s (%d) != %s (%d)",
                   b[[i]]$label, b[[i]]$n, ex$label, n_ex, b[[i + 1]]$label, b[[i + 1]]$n))
    }
    if (!is.null(ex$reasons) && sum(ex$reasons) != ex$n) {
      stop(sprintf("%s: 理由の合計 (%d) が除外人数 (%d) と一致しません。理由が重なっているなら階層的に振り分けてください",
                   ex$label, sum(ex$reasons), ex$n))
    }
  }
  invisible(flow)
}


# ---- 2. 図を組み立てる ----

# 人数の書き方 (JAMA 形式): 4 桁以下は区切りなし (1478)、5 桁以上は細いスペース (10 340)
fmt_n <- function(n, style = "n = %s") {
  s <- formatC(n, format = "d", big.mark = " ")
  sprintf(style, ifelse(abs(n) < 10000, gsub(" ", "", s, fixed = TRUE), s))
}

# 長い理由を折り返す。2 行目以降は行頭の点(•)の分だけ字下げする
wrap_reason <- function(x, width) {
  lines <- strwrap(x, width = width)
  paste0(c("• ", rep("  ", length(lines) - 1)), lines, collapse = "\n")
}

# fontsize   : 文字の大きさ(pt)。実寸で保存するので印刷時もこの大きさ。7-9 pt が目安
# wrap_width : 横の箱の理由を何文字で折り返すか
# n_style    : 人数の書式。"n = %s"(既定)、"n=%s"(NEJM など詰める投稿先)
# gap        : 箱と箱の間隔(文字の高さを 1 とした倍数)
# fontfamily : フォント。EMF に書き出すときは "Arial" と名前で指定する ("sans" のままだと Word が別の書体に置き換える)
build_flow <- function(flow, fontsize = 8, wrap_width = 45, n_style = "n = %s", gap = 1.5, fontfamily = "sans",
                       lwd = 0.75) {
  check_flow(flow)
  set_consort_defaults(
    txt_gp       = gpar(fontsize = fontsize, fontfamily = fontfamily, col = "black"),
    box_gp       = gpar(fill = "white", col = "black", lwd = lwd),
    arrow_gp     = gpar(col = "black", fill = "black", lwd = lwd),
    arrow_length = 0.05,
    arrow_type   = "closed",
    pad_u        = gap
  )

  # b$note があれば人数の下に行を足す (例: 解析コホートの結果不明の人数。この研究で追加)
  main_txt <- vapply(flow$boxes, function(b) {
    lab <- sub("31 March (\\d{4})", "March 31, \\1", b$label)
    paste0(lab, "\n(", fmt_n(b$n, n_style), ")", if (!is.null(b$note)) paste0("\n", b$note) else "")
  }, "")
  side_txt <- vapply(flow$exclusions, function(ex) {
    if (is.null(ex)) return(NA_character_)
    head <- paste0(ex$label, " (", fmt_n(ex$n, n_style), ")")
    if (is.null(ex$reasons)) return(head)
    items <- vapply(names(ex$reasons), function(nm) {
      nm_show <- sub("^Care level", "Care-need level", nm)
      wrap_reason(paste0(nm_show, " (", fmt_n(ex$reasons[[nm]], n_style), ")"), wrap_width)
    }, "")
    paste(c(head, items), collapse = "\n")
  }, "")

  # 縦の箱の幅を、いちばん広い箱にそろえる(consort は文字の長さで幅が変わるため)
  pdf(NULL)
  main_w <- max(vapply(main_txt, function(t) {
    convertWidth(grobWidth(textGrob(t, gp = gpar(fontsize = fontsize, fontfamily = fontfamily))), "mm", TRUE)
  }, 0)) + 2 * 3 * 1.4   # 左右の余白
  dev.off()
  min_w <- unit(main_w, "mm")
  main_box <- function(x = 0.5, y = 0.5, width, height, gp = gpar(), name = NULL, ...) {
    rectGrob(x = x, y = y, width = unit.pmax(width, min_w), height = height, gp = gp, name = name)
  }

  g <- add_box(txt = main_txt[1], box_fn = main_box)
  for (i in seq_along(side_txt)) {
    if (!is.na(side_txt[i])) g <- add_side_box(g, txt = side_txt[i], box_fn = rectGrob)
    g <- add_box(g, txt = main_txt[i + 1], box_fn = main_box)
  }
  attr(g, "fontsize") <- fontsize
  g
}

# 図の大きさ(mm)を consort の配置計算から求める。
# consort の座標は「文字の幅」単位で、グリッドの既定 12 pt が基準になっている。
# consort は上に pad/2、下に pad の余白を取るので、そのまま描くと下が空く。
# ここで箱の実際の範囲を求め、上下左右の余白を margin_mm にそろえる。
flow_geometry <- function(g, margin_mm = 2) {
  pdf(NULL)
  on.exit(dev.off())
  cc <- consort:::calc_coords(g)
  mm_per_char <- 12 * 25.4 / 72
  pad <- consort:::consort_opt("pad_u")
  last <- names(cc$y)[length(cc$y)]
  top    <- pad / 2                                               # 最初の箱の上端
  bottom <- cc$y[[last]] + get_coords(g[[last]]$box)$height / 2    # 最後の箱の下端
  list(
    width_mm  = cc$max_width * mm_per_char + 2 * margin_mm,
    height_mm = (bottom - top) * mm_per_char + 2 * margin_mm,
    vp_w      = cc$max_width  * mm_per_char / 0.98,   # consort は描画領域の 98% を使う
    vp_h      = cc$max_height * mm_per_char / 0.98,
    vp_shift  = 0.01 * cc$max_height * mm_per_char / 0.98 + top * mm_per_char - margin_mm
  )
}

# 余白を詰めて描く(plot(g) の代わりに使う)
draw_flow <- function(g, geo = flow_geometry(g)) {
  grid.newpage()
  pushViewport(viewport(y = unit(1, "npc") + unit(geo$vp_shift, "mm"), just = "top",
                        width = unit(geo$vp_w, "mm"), height = unit(geo$vp_h, "mm")))
  grid.draw(build_grid(g))
  popViewport()
}

flow_size_mm <- function(g) {
  geo <- flow_geometry(g)
  c(width = geo$width_mm, height = geo$height_mm)
}


# ---- 3. 保存 ----

# TIFF(600 dpi, LZW)は投稿用、PDF はベクターで拡大しても劣化しない(編集部に求められたとき用)。
# 大きさは既定で内容から自動。width_mm を 89(1 段)/ 183(2 段)に固定したいときは指定する
save_flow <- function(g, name, formats = c("tiff", "pdf"), dpi = 600, width_mm = NULL, height_mm = NULL,
                      dir = PATHS$figures) {
  geo <- flow_geometry(g)
  sz <- c(width = geo$width_mm, height = geo$height_mm)
  w <- if (is.null(width_mm)) sz[["width"]] else width_mm
  h <- if (is.null(height_mm)) sz[["height"]] else height_mm
  if (w > 183) warning(sprintf("図の幅が %.0f mm で 2 段(183 mm)を超えます。wrap_width を小さくするか fontsize を下げてください", w))
  if (!is.null(width_mm) && width_mm < sz[["width"]] - 1) {
    warning(sprintf("指定の幅 %.0f mm は内容に必要な幅 %.0f mm より狭く、箱がはみ出します", width_mm, sz[["width"]]))
  }

  paths <- character(0)
  if ("tiff" %in% formats) {
    p <- file.path(dir, paste0(name, ".tiff"))
    ragg::agg_tiff(p, width = w, height = h, units = "mm", res = dpi, compression = "lzw", background = "white")
    draw_flow(g, geo); dev.off()
    paths <- c(paths, p)
  }
  if ("pdf" %in% formats) {
    p <- file.path(dir, paste0(name, ".pdf"))
    grDevices::cairo_pdf(p, width = w / 25.4, height = h / 25.4)   # cairo は • などの記号も埋め込める
    draw_flow(g, geo); dev.off()
    paths <- c(paths, p)
  }
  if ("png" %in% formats) {   # 確認用・スライド用
    p <- file.path(dir, paste0(name, ".png"))
    ragg::agg_png(p, width = w, height = h, units = "mm", res = dpi, background = "white")
    draw_flow(g, geo); dev.off()
    paths <- c(paths, p)
  }
  message(sprintf("[flow] 保存: %s (%.0f x %.0f mm, %d dpi)", paste(basename(paths), collapse = ", "), w, h, dpi))
  invisible(paths)
}


# ---- 4. 数値突合用の表 ----
# 図に出した人数を 1 行 1 項目で書き出す。原稿の Results と照合するとき(numbers-consistency-check)に使う
flow_counts_table <- function(flow) {
  rows <- list()
  for (i in seq_along(flow$boxes)) {
    rows[[length(rows) + 1]] <- data.frame(level = "box", label = flow$boxes[[i]]$label, n = flow$boxes[[i]]$n)
    if (i <= length(flow$exclusions) && !is.null(ex <- flow$exclusions[[i]])) {
      rows[[length(rows) + 1]] <- data.frame(level = "exclusion", label = ex$label, n = ex$n)
      if (!is.null(ex$reasons)) {
        rows[[length(rows) + 1]] <- data.frame(level = "reason", label = names(ex$reasons), n = as.integer(ex$reasons))
      }
    }
  }
  do.call(rbind, rows)
}
