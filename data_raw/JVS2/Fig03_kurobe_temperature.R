# Fig03_kurobe_temperature.R
# Fig. 3: Changes in temperature at the Kurobe Dam (1965-2023)
# Daily maximum, mean, and minimum temperatures with regression lines
#
# Required: kurobe_dam_temperature.csv (or kurobe_dam_temperature object)
# Required: theme_tateyama() from theme_tateyama.R
# ---------------------------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)

# ── 共通テーマ・カラー（theme_tateyama.R と共通） ────────────
theme_tateyama <- function(base_size =10) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title        = element_text(size = base_size),
      axis.text         = element_text(size = base_size * 0.9),
      axis.line         = element_line(colour = "black", linewidth = 0.4),
      axis.ticks        = element_line(colour = "black", linewidth = 0.4),
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size * 0.9),
      legend.key.size   = unit(0.45, "cm"),
      legend.background = element_rect(fill = "white", colour = NA),
      panel.border      = element_blank(),
      plot.margin       = margin(4, 6, 4, 4, "mm")
    )
}

# ── データ読み込み ────────────────────────────────────────────
# CSVから読む場合:
# d <- read.csv("kurobe_dam_temperature.csv")
# パッケージオブジェクトを使う場合:
d <- kurobe_dam_temperature   # 列: year, max, mean, min

# ── Wide → Long 変換 ─────────────────────────────────────────
d_long <- d |>
  select(year, max, mean, min) |>
  pivot_longer(
    cols      = c(max, mean, min),
    names_to  = "variable",
    values_to = "temp"
  ) |>
  mutate(
    variable = factor(variable,
                      levels = c("max", "mean", "min"),
                      labels = c("Daily maximum",
                                 "Daily mean",
                                 "Daily minimum"))
  )

# ── 回帰直線用データ（geom_smooth の代わりに明示的に計算） ────
lm_lines <- d_long |>
  group_by(variable) |>
  summarise(
    slope     = coef(lm(temp ~ year))[2],
    intercept = coef(lm(temp ~ year))[1],
    .groups   = "drop"
  ) |>
  mutate(
    x_min = min(d$year),
    x_max = max(d$year),
    y_min = intercept + slope * x_min,
    y_max = intercept + slope * x_max
  )

# ── カラー・記号設定 ──────────────────────────────────────────
temp_col   <- c("Daily maximum" = "#CC0000",
                "Daily mean"    = "black",
                "Daily minimum" = "#0055AA")
temp_shape <- c("Daily maximum" = 24,   # 上向き三角（塗り）
                "Daily mean"    = 16,   # 丸
                "Daily minimum" = 25)   # 下向き三角（塗り）

# ── プロット ──────────────────────────────────────────────────
fig3 <- ggplot(d_long, aes(x = year, y = temp,
                            colour = variable,
                            shape  = variable,
                            fill   = variable)) +

  # 観測値（折れ線＋点）
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_point(size = 1.8, stroke = 0.3) +

  # 回帰直線
  geom_segment(
    data = lm_lines,
    aes(x = x_min, xend = x_max,
        y = y_min, yend = y_max,
        colour = variable),
    linewidth = 1.0,
    inherit.aes = FALSE
  ) +

  # スケール
  scale_colour_manual(values = temp_col) +
  scale_fill_manual(values = temp_col) +
  scale_shape_manual(values = temp_shape) +
  scale_x_continuous(
    breaks = seq(1965, 2025, by = 5),
    limits = c(1964, 2024),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0, 14, by = 2),
    limits = c(0, 14),
    expand = expansion(mult = c(0.02, 0.02))
  ) +

  # 軸ラベル
  labs(
    x = "Year",
    y = "Temperature (\u00b0C)"
  ) +

  # テーマ
  theme_tateyama(base_size = 14) +
  theme(
    legend.position  = c(0.02, 0.98),   # 左上（後で調整可）
    legend.justification = c(0, 1),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(fig3)

# ── 保存（査読中：PNG） ───────────────────────────────────────
ggsave("Fig03_kurobe_temperature.png",
       plot   = fig3,
       width  = 174, height = 120, units = "mm",
       dpi    = 300, bg = "white")

#── 保存（受理後：PDF、Ubuntu推奨） ──────────────────────────
ggsave("Fig03_kurobe_temperature.pdf",
       plot   = fig3,
       width  = 174, height = 120, units = "mm",
       device = cairo_pdf)

# ── 回帰統計の確認 ────────────────────────────────────────────
cat("\n=== Regression statistics (Table 2) ===\n")
for (v in c("Daily maximum", "Daily mean", "Daily minimum")) {
  sub <- d_long[d_long$variable == v, ]
  m   <- lm(temp ~ year, data = sub)
  sm  <- summary(m)
  cat(sprintf(
    "%s: slope = %.4f\u00b0C/yr (%.3f\u00b0C/decade), R\u00b2 = %.3f, p = %.2e\n",
    v,
    coef(m)[2],
    coef(m)[2] * 10,
    sm$r.squared,
    pf(sm$fstatistic[1], sm$fstatistic[2],
       sm$fstatistic[3], lower.tail = FALSE)
  ))
}
