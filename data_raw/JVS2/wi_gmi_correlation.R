# ============================================================
# WI・気温 vs. A. mariesii G, M, I 相関解析
# ============================================================

d <- read.csv("TemperatureWIAbiesPopulation.csv")
d$plot <- factor(d$plot, levels = c("Kaminokodaira", "Matsuotoge", "Kagamiishi"))

# 気候変数・人口動態率の列名
clim_vars <- c("WI", "Tmax", "Tmin", "Tmean")
demo_vars <- c("p", "m", "r")
demo_labs <- c(p = "Growth (G)", m = "Mortality (M)", r = "Ingrowth (I)")

# ============================================================
# 相関解析関数
# ============================================================
corr_summary <- function(data, clim, demo) {
  ct <- cor.test(data[[clim]], data[[demo]], method = "pearson")
  data.frame(
    climate  = clim,
    response = demo_labs[demo],
    n        = nrow(data),
    r        = round(ct$estimate, 3),
    p        = round(ct$p.value, 4)
  )
}

# ============================================================
# 1. 各プロット別相関（n=6）
# ============================================================
cat("============================================================\n")
cat("1. Pearson correlation by plot (n=6)\n")
cat("============================================================\n")

results_plot <- data.frame()
for (plot in levels(d$plot)) {
  cat(sprintf("\n--- %s ---\n", plot))
  df <- d[d$plot == plot, ]
  res <- data.frame()
  for (clim in clim_vars) {
    for (demo in demo_vars) {
      res <- rbind(res, corr_summary(df, clim, demo))
    }
  }
  res$plot <- plot
  print(res[, c("climate", "response", "r", "p")], row.names = FALSE)
  results_plot <- rbind(results_plot, res)
}

# ============================================================
# 2. プール解析（n=18）
# ============================================================
cat("\n============================================================\n")
cat("2. Pearson correlation pooled (n=18)\n")
cat("============================================================\n")

results_pool <- data.frame()
for (clim in clim_vars) {
  for (demo in demo_vars) {
    results_pool <- rbind(results_pool, corr_summary(d, clim, demo))
  }
}
print(results_pool[, c("climate", "response", "r", "p")], row.names = FALSE)

# ============================================================
# 3. WI vs. M に絞った要約（論文用）
# ============================================================
cat("\n============================================================\n")
cat("3. WI vs. Mortality (M) summary\n")
cat("============================================================\n")

cat("\n--- By plot (n=6) ---\n")
for (plot in levels(d$plot)) {
  df <- d[d$plot == plot, ]
  ct <- cor.test(df$WI, df$m, method = "pearson")
  cat(sprintf("%-15s r = %+.3f, p = %.4f\n", plot, ct$estimate, ct$p.value))
}

cat("\n--- Pooled (n=18) ---\n")
ct <- cor.test(d$WI, d$m, method = "pearson")
cat(sprintf("%-15s r = %+.3f, p = %.4f\n", "Pooled", ct$estimate, ct$p.value))

cat("\n=== Analysis complete ===\n")
