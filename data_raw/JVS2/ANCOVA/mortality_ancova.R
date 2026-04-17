# ============================================================
# Cumulative mortality analysis of Abies mariesii
# ANCOVA: death_ratio ~ year * plot
# ============================================================

# --- 関数定義 ------------------------------------------------
MortalityRate <- function(p, d, method = c("instantaneous", "discrete")) {
  method <- match.arg(method)
  switch(method,
    "instantaneous" = log(1 / p) / d,   # ln(N0/Ns)/t
    "discrete"      = 1 - (1 - p)^(1 / d)
  )
}

# --- データ読み込み ------------------------------------------
load("Abies_death_ratio.RData")   # Abies_death_ratio (list)
wi <- read.csv("wi2024.csv")
names(wi)[1] <- "year"

# --- 各プロットにWIを結合 ------------------------------------
get_data <- function(plot) {
  df      <- Abies_death_ratio[[plot]]
  df$WI   <- wi[match(df$year, wi$year), plot]
  df$plot <- plot
  df
}

Kami  <- get_data("Kaminokodaira")
Matsu <- get_data("Matsuotoge")
Kaga  <- get_data("Kagamiishi")

all_data       <- rbind(Kami, Matsu, Kaga)
all_data$plot  <- factor(all_data$plot,
                         levels = c("Kaminokodaira", "Matsuotoge", "Kagamiishi"))

# ============================================================
# 1. WI vs. 累積死亡率  Pearson相関（各プロット n=7）
# ============================================================
cat("=== 1. Pearson correlation: WI vs. cumulative mortality ===\n")
for (plot in levels(all_data$plot)) {
  df <- all_data[all_data$plot == plot, ]
  ct <- cor.test(df$WI, df$death_ratio, method = "pearson")
  cat(sprintf("%-15s r = %6.3f, p = %.4f\n", plot, ct$estimate, ct$p.value))
}

cat("\n--- Pooled (n=21) ---\n")
ct_all <- cor.test(all_data$WI, all_data$death_ratio, method = "pearson")
cat(sprintf("Pooled         r = %6.3f, p = %.4f\n", ct_all$estimate, ct_all$p.value))

# ============================================================
# 2. ANCOVA: 累積死亡率 ~ year * plot
# ============================================================
cat("\n=== 2. ANCOVA: death_ratio ~ year * plot ===\n")
m <- lm(death_ratio ~ year * plot, data = all_data)
print(summary(m))

# --- 各プロットの回帰直線の傾き ---
b_kami  <- coef(m)["year"]
b_matsu <- coef(m)["year"] + coef(m)["year:plotMatsuotoge"]
b_kaga  <- coef(m)["year"] + coef(m)["year:plotKagamiishi"]

cat("--- Slope of each plot (cumulative mortality/yr) ---\n")
cat(sprintf("Kaminokodaira (Ecotone)  : %.6f\n", b_kami))
cat(sprintf("Matsuotoge    (Subarctic): %.6f\n", b_matsu))
cat(sprintf("Kagamiishi    (Timberline): %.6f\n", b_kaga))

# ============================================================
# 3. ペアワイズ傾きの比較（参照水準を変えて再フィット）
# ============================================================
cat("\n=== 3. Pairwise slope comparisons ===\n")

pairwise_slope <- function(data, ref, other) {
  data$pl <- factor(data$plot, levels = c(ref, setdiff(levels(data$plot), ref)))
  m_tmp   <- lm(death_ratio ~ year * pl, data = data)
  cf      <- summary(m_tmp)$coefficients
  row     <- paste0("year:pl", other)
  c(estimate = cf[row, "Estimate"],
    t        = cf[row, "t value"],
    p        = cf[row, "Pr(>|t|)"])
}

pairs <- list(
  c("Kaminokodaira", "Matsuotoge"),
  c("Kaminokodaira", "Kagamiishi"),
  c("Matsuotoge",    "Kagamiishi")
)

results <- data.frame()
for (pair in pairs) {
  res <- pairwise_slope(all_data, pair[1], pair[2])
  cat(sprintf("%s vs. %s:\n  slope diff = %.6f, t = %.3f, p = %.4f",
              pair[1], pair[2], res["estimate"], res["t"], res["p"]))
  # Bonferroni補正
  p_bonf <- min(res["p"] * 3, 1)
  cat(sprintf(", p_Bonferroni = %.4f\n", p_bonf))
  results <- rbind(results, data.frame(
    comparison    = paste(pair[1], "vs", pair[2]),
    slope_diff    = round(res["estimate"], 6),
    t_value       = round(res["t"], 3),
    p_value       = round(res["p"], 4),
    p_Bonferroni  = round(p_bonf, 4)
  ))
}

cat("\n--- Summary table ---\n")
print(results, row.names = FALSE)

