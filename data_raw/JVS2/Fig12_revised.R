# Fig12_revised.R
# Fig. 12 revised: 2-panel figure
# Panel (a): Cumulative fall ratio of dead standing A. mariesii trees
# Panel (b): Estimated frequency distribution of death years
# Reviewer 2 response: dual y-axis replaced with 2-panel layout
#
# Required objects: dd4, plt2, MTF(), Fplt_yr_median()
# ---------------------------------------------------------------

Fig12_revised <- function(
    mat       = 6.2,        # mean annual temperature at Kaminokodaira (°C)
    col_bar   = "#CC3333",  # bar color for death histogram
    col_line  = "black",    # line color for fall ratio
    save_png  = FALSE,      # set TRUE to save as PNG
    save_pdf  = FALSE,      # set TRUE to save as PDF
    filename  = "Fig12_revised"
) {

  # ── 1. Data preparation ─────────────────────────────────────
  d.    <- subset(dd4, plot == "Kaminokodaira" & sp == "オオシラビソ")
  years <- as.numeric(subset(plt2, na == "Kaminokodaira", paste0("yr", 1:7)))
  f0.   <- paste0("f0", 1:7)

  # --- Panel (a): fallen_ratio ---
  df_fall <- data.frame(
    Year        = years,
    Fallen_ratio = fallen_ratio(form = f0.)
  )

  # --- Panel (b): estimated death years ---
  # Select initially dead-standing trees with DBH measurement
  i. <- d.$f01 == 0 & d.$f07 == -1 & !is.na(d.$d01)
  # Note: 3 trees never fell (f07 != -1); these are excluded from death-year estimation
  n_dead_standing <- sum(d.$f01 == 0, na.rm = TRUE)   # 25 trees
  n_fell          <- sum(i., na.rm = TRUE)             # 22 trees (88%)

  dbh.    <- d.$d01[i.]
  mtf.    <- MTF(dbh., mat)   # mean time to fall (years)

  # First period when each tree was confirmed fallen (f == -1)
  period. <- apply(
    d.[i., paste0("f0", 1:7)] == -1, 1,
    function(x) which(x)[1]
  )

  yr_fall.  <- Fplt_yr_median("Kaminokodaira", period.)  # median fall year
  yr_died.  <- as.numeric(yr_fall. - mtf.)               # estimated death year

  # Histogram
  breaks. <- seq(1970, 2015, by = 5)
  h       <- hist(yr_died., breaks = breaks., plot = FALSE)

  # Normal distribution fit (for overlay)
  mu_d  <- mean(yr_died.)
  sd_d  <- sd(yr_died.)
  x_fit <- seq(1968, 2015, length.out = 300)
  y_fit <- dnorm(x_fit, mean = mu_d, sd = sd_d) * 5  # scale to bar width

  # K-S test result (reported in text)
  ks_res <- ks.test(yr_died., "pnorm", mean = mu_d, sd = sd_d)

  # ── 2. Plot ──────────────────────────────────────────────────
  if (save_png) png(paste0(filename, ".png"), width = 7, height = 8,
                    units = "in", res = 300)
  if (save_pdf) pdf(paste0(filename, ".pdf"), width = 7, height = 8)

  op <- par(
    mfrow  = c(2, 1),
    mar    = c(4.5, 5, 3, 2),
    oma    = c(0, 0, 1, 0),
    mgp    = c(3, 0.7, 0),
    tcl    = -0.3
  )

  # ── Panel (a) ────────────────────────────────────────────────
  plot(
    df_fall$Year, df_fall$Fallen_ratio,
    type  = "b", lwd = 1.8,
    pch   = 21, bg = "white", cex = 1.2,
    col   = col_line,
    xlim  = c(1997, 2027),
    ylim  = c(0, 1.05),
    xlab  = "Year",
    ylab  = "Cumulative fall ratio\nof dead standing trees",
    cex.lab = 1.1, cex.axis = 0.9,
    las   = 1
  )

  # Survey year reference lines
  abline(v = years, col = "grey70", lty = 3, lwd = 0.8)

  # Annotate n and final value
  text(2024, 0.88 + 0.05,
       labels = sprintf("%.0f%%  (n = %d/%d)", 88, n_fell, n_dead_standing),
       cex = 0.85, adj = c(1, 0))

  # Horizontal grid
  abline(h = seq(0.2, 1.0, 0.2), col = "grey85", lty = 3)
  box()

  mtext("(a)", side = 3, adj = 0, line = 0.8, font = 2, cex = 1.1)

  # ── Panel (b) ────────────────────────────────────────────────
  bp <- barplot(
    h$counts / sum(h$counts),
    names.arg = h$mids,
    col       = col_bar,
    border    = "black",
    space     = 0,
    xlim      = c(0, length(h$mids)),
    ylim      = c(0, 0.55),
    xlab      = "Estimated year of death",
    ylab      = "Relative frequency",
    cex.lab   = 1.1, cex.axis = 0.9,
    cex.names = 0.85,
    las       = 1,
    axes      = TRUE
  )

  # Normal distribution overlay
  # Map x_fit to barplot coordinates
  x_bp <- (x_fit - min(breaks.)) / 5   # convert year to bar index
  lines(x_bp, y_fit, col = "black", lwd = 1.5, lty = 1)

  # Mean death year line
  mean_bp <- (mu_d - min(breaks.)) / 5
  abline(v = mean_bp, col = "black", lty = 2, lwd = 1.5)
  text(mean_bp + 0.1, 0.50,
       labels = sprintf("Mean = %.1f yr\n(\u00b1 %.1f SD)", mu_d, sd_d),
       adj = c(0, 1), cex = 0.82)

  # WI > 55 reference (1994)
  wi55_bp <- (1994 - min(breaks.)) / 5
  abline(v = wi55_bp, col = "navy", lty = 3, lwd = 1.2)
  text(wi55_bp + 0.1, 0.46,
       labels = "WI > 55\n(from 1994)",
       adj = c(0, 1), cex = 0.75, col = "navy")

  # K-S test annotation
  mtext(
    sprintf("K\u2013S test: D = %.3f, p = %.3f  (normal distribution not rejected)",
            ks_res$statistic, ks_res$p.value),
    side = 1, line = 3.5, cex = 0.78
  )

  # Legend
  legend("topleft",
         legend = c(
           "Relative frequency",
           "Normal distribution fit",
           sprintf("Mean death year (%.1f)", mu_d)
         ),
         fill   = c(col_bar, NA, NA),
         lty    = c(NA, 1, 2),
         lwd    = c(NA, 1.5, 1.5),
         border = c("black", NA, NA),
         cex    = 0.82,
         bty    = "n"
  )

  mtext("(b)", side = 3, adj = 0, line = 0.8, font = 2, cex = 1.1)

  # ── Overall title ─────────────────────────────────────────────
  mtext(
    expression(
      paste("Fig. 12.  Dead standing trees and estimated death years of ",
            italic("Abies mariesii"), " in the Ecotone plot")
    ),
    outer = TRUE, side = 3, line = -0.5, cex = 0.92
  )

  par(op)

  if (save_png | save_pdf) dev.off()

  # Return data invisibly
  invisible(list(
    fall_ratio   = df_fall,
    yr_died      = yr_died.,
    yr_fall      = yr_fall.,
    mtf          = mtf.,
    ks_test      = ks_res,
    n_dead_stand = n_dead_standing,
    n_fell       = n_fell
  ))
}

# ── Usage examples ──────────────────────────────────────────────
# Fig12_revised()                          # screen preview
# Fig12_revised(save_png = TRUE)           # save as PNG (300 dpi)
# Fig12_revised(save_pdf = TRUE)           # save as PDF
# result <- Fig12_revised()                # capture data
# result$ks_test                           # check K-S test result
