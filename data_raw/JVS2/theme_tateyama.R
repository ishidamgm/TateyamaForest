# theme_tateyama.R
# 全図共通テーマ・カラー・凡例スタイル

library(ggplot2)

# ── 共通テーマ ───────────────────────────────
theme_tateyama <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      # 軸
      axis.title   = element_text(size = base_size),
      axis.text    = element_text(size = base_size * 0.9),
      axis.line    = element_line(colour = "black", linewidth = 0.4),
      axis.ticks   = element_line(colour = "black", linewidth = 0.4),
      # 凡例
      legend.title     = element_blank(),
      legend.text      = element_text(size = base_size * 0.9),
      legend.key.size  = unit(0.4, "cm"),
      legend.background= element_rect(fill = "white", colour = NA),
      # パネル
      panel.border     = element_blank(),
      strip.background = element_blank(),
      strip.text       = element_text(size = base_size, face = "bold"),
      # マージン
      plot.margin = margin(4, 6, 4, 4, "mm")
    )
}

# ── プロット共通カラー・記号 ──────────────────
plt_col <- c(
  "Temperate plot"  = "darkolivegreen4",
  "Ecotone plot"    = "blue",
  "Subarctic plot"  = "blueviolet",
  "Timberline plot" = "cyan3"
)

plt_shape <- c(
  "Temperate plot"  = 16,
  "Ecotone plot"    = 17,
  "Subarctic plot"  = 15,
  "Timberline plot" = 18
)

plt_lty <- c(
  "Temperate plot"  = "solid",
  "Ecotone plot"    = "solid",
  "Subarctic plot"  = "solid",
  "Timberline plot" = "solid"
)

# ── ggsave用の出力設定 ───────────────────────
# 査読中：PNG 300dpi
save_fig_png <- function(p, filename, width = 174, height = 140) {
  ggsave(filename, plot = p,
         width = width, height = height, units = "mm",
         dpi = 300, bg = "white")
}

# 受理後：cairo_pdf（Ubuntu推奨）
save_fig_pdf <- function(p, filename, width = 174, height = 140) {
  ggsave(filename, plot = p,
         width = width, height = height, units = "mm",
         device = cairo_pdf)
}
