# ============================================================
# 黒部（欠測） vs. 富山の気温 単回帰分析
# 目的：欠測値推定用の回帰式を作成する
# ============================================================

df <- read.csv("kurobe_toyama.csv")   # ← ファイルパスを適宜変更

# ------------------------------------------------------------
# 1. 回帰モデルの作成
# ------------------------------------------------------------
fit_min  <- lm(min  ~ toya_Tmin,  data = df)
fit_max  <- lm(max  ~ toya_Tmax,  data = df)
fit_mean <- lm(mean ~ toya_Tmean, data = df)

# ------------------------------------------------------------
# 2. 結果の表示
# ------------------------------------------------------------
cat("=== min ~ toya_Tmin ===\n");  print(summary(fit_min))
cat("=== max ~ toya_Tmax ===\n");  print(summary(fit_max))
cat("=== mean ~ toya_Tmean ===\n"); print(summary(fit_mean))

# ------------------------------------------------------------
# 3. 散布図 + 回帰直線の描画
# ------------------------------------------------------------
png("kurobe_regression_plots.png", width = 1800, height = 600, res = 150)
par(mfrow = c(1, 3))

plot(df$toya_Tmin, df$min, pch = 16, col = "steelblue",
     xlab = "富山 Tmin (°C)", ylab = "黒部 min (°C)",
     main = sprintf("min ~ toya_Tmin\ny = %.4f + %.4f x,  R² = %.4f",
                    coef(fit_min)[1], coef(fit_min)[2], summary(fit_min)$r.squared))
abline(fit_min, col = "red", lwd = 2)

plot(df$toya_Tmax, df$max, pch = 16, col = "steelblue",
     xlab = "富山 Tmax (°C)", ylab = "黒部 max (°C)",
     main = sprintf("max ~ toya_Tmax\ny = %.4f + %.4f x,  R² = %.4f",
                    coef(fit_max)[1], coef(fit_max)[2], summary(fit_max)$r.squared))
abline(fit_max, col = "red", lwd = 2)

plot(df$toya_Tmean, df$mean, pch = 16, col = "steelblue",
     xlab = "富山 Tmean (°C)", ylab = "黒部 mean (°C)",
     main = sprintf("mean ~ toya_Tmean\ny = %.4f + %.4f x,  R² = %.4f",
                    coef(fit_mean)[1], coef(fit_mean)[2], summary(fit_mean)$r.squared))
abline(fit_mean, col = "red", lwd = 2)

dev.off()
cat("プロット保存: kurobe_regression_plots.png\n")

# ------------------------------------------------------------
# 4. 欠測値の推定（富山データが既知の年を指定）
# ------------------------------------------------------------
# 例：2003年（H15）と 2005年（H17）の富山気温が既知の場合

# year Tmean Tmax Tmin
# 65 2003  14.1 18.5 10.5
# 66 2004  15.2 19.9 11.2
# 76 2014  14.3 18.7 10.4
#    2024  15.9 20.4 12.2

missing_years <- data.frame(
  year       = c(2003, 2005,2014,2024),         # ← 欠測年
  toya_Tmin  = c(10.5,   11.2,10.4,   12.2),           # ← 富山の実測値を入力
  toya_Tmax  = c(18.5,   19.9,18.7,   20.4),
  toya_Tmean = c(14.1,   15.2,14.3,   15.9)
)

# NA以外の行だけ推定（値を入力した後に実行）
#if (any(!is.na(missing_years$toya_Tmin))) {
  missing_years$est_min  <- predict(fit_min,  newdata = missing_years)
  missing_years$est_max  <- predict(fit_max,  newdata = missing_years)
  missing_years$est_mean <- predict(fit_mean, newdata = missing_years)
  print(missing_years)
#}

