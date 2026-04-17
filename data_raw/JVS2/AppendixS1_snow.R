# AppendixS1_snow.R
# Appendix S1: Annual maximum snow depth and snow cover duration
# 2-panel figure combining original Fig. 5 and Fig. 6
# Revised for submission to Journal of Vegetation Science
#
# Required objects: snow (data frame with columns: pl, yr, dep, pe)
# ---------------------------------------------------------------

#' Title
#'
#' @param dat
#' @param save_png
#' @param save_pdf
#' @param filename
#' @param width
#' @param height
#'
#' @returns
#' @export
#'
#' @examples
#' #' ── Usage ────────────────────────────────────────────────────────
#' AppendixS1_snow()                        # screen preview
#' AppendixS1_snow(save_png = TRUE)         # PNG 300 dpi
#' AppendixS1_snow(save_pdf = TRUE)         # PDF
#'
#'
#'
AppendixS1_snow <- function(
    dat      = snow,
    save_png = FALSE,
    save_pdf = FALSE,
    filename = "AppendixS1_snow",
    width    = 7,
    height   = 8
) {

  # ── Plot–label mapping ───────────────────────────────────────
  # Japanese site names → English plot names
  pl_map <- c(
    "\u30d6\u30ca\u5742"   = "Temperate plot",    # Bunazaka
    "\u4e0a\u30ce\u5c0f\u5e73" = "Ecotone plot",  # Kaminokodaira
    "\u677e\u5c3e\u5cf0"  = "Subarctic plot",     # Matsuotoge
    "\u9e1f\u77f3"        = "Timberline plot"     # Kagamiishi
  )

  # ── Common style ─────────────────────────────────────────────
  # Plot order: Timberline → Subarctic → Ecotone → Temperate (high to low elevation)
  # Colors: consistent with Fig. 12 style (dark to light with elevation)
  style <- data.frame(
    pl       = c("\u30d6\u30ca\u5742", "\u4e0a\u30ce\u5c0f\u5e73",
                 "\u677e\u5c3e\u5cf0", "\u9e1f\u77f3"),
    label    = c("Temperate plot", "Ecotone plot",
                 "Subarctic plot", "Timberline plot"),
    col      = c("darkolivegreen4", "blue", "blueviolet", "cyan3"),
    pch      = c(16, 17, 15, 18),
    lty      = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  # ── Open device ──────────────────────────────────────────────
  if (save_png) png(paste0(filename, ".png"),
                    width = width, height = height,
                    units = "in", res = 300, bg = "white")
  if (save_pdf) pdf(paste0(filename, ".pdf"),
                    width = width, height = height)

  op <- par(
    mfrow = c(2, 1),
    mar   = c(4.5, 5.5, 3.5, 2),
    oma   = c(0, 0, 1.5, 0),
    mgp   = c(3.2, 0.7, 0),
    tcl   = -0.3,
    las   = 1
  )

  # ══ Panel (a): Maximum snow depth ════════════════════════════
  # Sites: Temperate, Ecotone, Subarctic (Timberline not measured)
  depth_pls <- c("\u30d6\u30ca\u5742", "\u4e0a\u30ce\u5c0f\u5e73", "\u677e\u5c3e\u5cf0")

  gd_dep <- subset(dat, pl %in% depth_pls & yr >= 2004 & !is.na(dep))

  yr_range <- range(gd_dep$yr)
  dep_max  <- ceiling(max(gd_dep$dep, na.rm = TRUE) / 100) * 100

  plot(NA,
       xlim = yr_range + c(-0.5, 0.5),
       ylim = c(0, dep_max),
       xlab = "Year",
       ylab = "Maximum snow depth (cm)",
       cex.lab = 1.15, cex.axis = 0.9,
       bty = "l"
  )

  # Horizontal grid
  abline(h = seq(100, dep_max, 100), col = "grey88", lty = 3)
  box(bty = "l")

  for (i in seq_len(nrow(style))) {
    cpl <- style$pl[i]
    if (!cpl %in% depth_pls) next
    gds <- gd_dep[gd_dep$pl == cpl, ]
    gds <- gds[order(gds$yr), ]
    lines(gds$yr, gds$dep,
          type = "b", lwd = 1.6,
          col  = style$col[i],
          pch  = style$pch[i],
          lty  = style$lty[i],
          cex  = 1.1)
  }

  # Legend (depth: 3 plots)
  dep_idx <- which(style$pl %in% depth_pls)
  legend("topright",
         legend = style$label[dep_idx],
         col    = style$col[dep_idx],
         pch    = style$pch[dep_idx],
         lty    = style$lty[dep_idx],
         lwd    = 1.6,
         cex    = 0.88,
         bty    = "n"
  )

  mtext("(a)", side = 3, adj = 0, line = 1.2, font = 2, cex = 1.1)

  # ══ Panel (b): Snow cover duration ═══════════════════════════
  # Sites: all 4 plots
  cover_pls <- c("\u30d6\u30ca\u5742", "\u4e0a\u30ce\u5c0f\u5e73",
                 "\u677e\u5c3e\u5cf0", "\u9e1f\u77f3")

  gd_pe <- subset(dat, pl %in% cover_pls & yr >= 1999 & !is.na(pe))

  yr_range2 <- range(gd_pe$yr)
  pe_max    <- ceiling(max(gd_pe$pe, na.rm = TRUE) / 50) * 50 + 10

  plot(NA,
       xlim = yr_range2 + c(-0.5, 0.5),
       ylim = c(0, pe_max),
       xlab = "Year",
       ylab = "Snow cover duration (days)",
       cex.lab = 1.15, cex.axis = 0.9,
       bty = "l"
  )

  # Horizontal grid
  abline(h = seq(50, pe_max, 50), col = "grey88", lty = 3)
  box(bty = "l")

  for (i in seq_len(nrow(style))) {
    cpl <- style$pl[i]
    if (!cpl %in% cover_pls) next
    gds <- gd_pe[gd_pe$pl == cpl, ]
    gds <- gds[order(gds$yr), ]
    lines(gds$yr, gds$pe,
          type = "b", lwd = 1.6,
          col  = style$col[i],
          pch  = style$pch[i],
          lty  = style$lty[i],
          cex  = 1.1)
  }

  # Legend (cover: 4 plots)
  legend("topright",
         legend = style$label,
         col    = style$col,
         pch    = style$pch,
         lty    = style$lty,
         lwd    = 1.6,
         cex    = 0.88,
         bty    = "n"
  )

  mtext("(b)", side = 3, adj = 0, line = 1.2, font = 2, cex = 1.1)

  # ── Overall title ─────────────────────────────────────────────
  mtext(
    "Appendix S1.  Annual maximum snow depth and snow cover duration at each plot",
    outer = TRUE, side = 3, line = 0.3, cex = 0.95
  )

  par(op)
  if (save_png | save_pdf) dev.off()

  # Return data invisibly
  invisible(list(depth = gd_dep, cover = gd_pe))
}


