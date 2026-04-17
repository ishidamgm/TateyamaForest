# ============================================================
# Table 6. Pearson correlation coefficients
#          between climate variables and demographic rates
#          of A. mariesii
# 入力: TemperatureWIAbiesPopulation.csv
# 出力: Table6_updated.csv
# ============================================================

# ------ 1. データ読み込み ------
d <- read.csv("TemperatureWIAbiesPopulation.csv")
d$plot <- factor(d$plot,
                 levels = c("Kaminokodaira", "Matsuotoge", "Kagamiishi"))

# ------ 2. 変数定義 ------
clim_vars   <- c("WI", "Tmax", "Tmin", "Tmean")
demo_vars   <- c("p", "m", "r")
plot_labels <- c(Kaminokodaira = "Ecotone",
                 Matsuotoge    = "Subarctic",
                 Kagamiishi    = "Timberline")

# ------ 3. ユーティリティ関数 ------

# 有意水準に応じたアスタリスクを返す
sig_star <- function(p) {
  if      (p < 0.01) "**"
  else if (p < 0.05) "*"
  else               ""
}

# r値と p値をセル文字列にフォーマット（例: "0.957**"）
fmt_cell <- function(r, p) {
  sprintf("%s%s", formatC(r, format = "f", digits = 3), sig_star(p))
}

# 1行分（1気候変数 × 3人口動態率）の相関を計算してフォーマット
corr_row <- function(data, clim) {
  sapply(demo_vars, function(dv) {
    ct <- cor.test(data[[clim]], data[[dv]], method = "pearson")
    fmt_cell(round(ct$estimate, 3), ct$p.value)
  })
}

# ------ 4. 表の組み立て ------
rows <- list()

# プロット別（n=6）
for (plt in levels(d$plot)) {
  df <- d[d$plot == plt, ]
  n  <- nrow(df)
  for (i in seq_along(clim_vars)) {
    cr <- corr_row(df, clim_vars[i])
    rows[[length(rows) + 1]] <- data.frame(
      Plot               = if (i == 1) sprintf("%s (n=%d)", plot_labels[plt], n) else "",
      "Climate variables" = clim_vars[i],
      "Growth (G)"       = cr["p"],
      "Mortality (M)"    = cr["m"],
      "Ingrowth (I)"     = cr["r"],
      check.names = FALSE
    )
  }
}

# プール（n=18）
for (i in seq_along(clim_vars)) {
  cr <- corr_row(d, clim_vars[i])
  rows[[length(rows) + 1]] <- data.frame(
    Plot               = if (i == 1) sprintf("Pooled (n=%d)", nrow(d)) else "",
    "Climate variables" = clim_vars[i],
    "Growth (G)"       = cr["p"],
    "Mortality (M)"    = cr["m"],
    "Ingrowth (I)"     = cr["r"],
    check.names = FALSE
  )
}

tbl <- do.call(rbind, rows)
rownames(tbl) <- NULL

# ------ 5. コンソール表示 ------
cat("Table 6. Pearson correlation coefficients\n")
cat("* p <0.05, ** p <0.01\n\n")
print(tbl, row.names = FALSE)

# ------ 6. CSV書き出し ------
write.csv(tbl, "Table6_updated.csv", row.names = FALSE)
message("Output saved: Table6_updated.csv")
