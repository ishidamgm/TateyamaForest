# Fig04_warmth_index.R  (v3)
# Fig. 4: Changes in the warmth index for each plot over time
# - Zone labels (Temperate/Ecotone/Subarctic) at 16pt between boundary lines
# - Line styles differ for B&W printing
# - Colors: orange / purple / cyan / dark-grey
#
# Required objects: WI_with_KurodeDamObservation, plt2
# Required: theme_tateyama() from theme_tateyama.R
# ---------------------------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)

# ── 共通テーマ ────────────────────────────────────────────────
theme_tateyama <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title        = element_text(size = base_size),
      axis.text         = element_text(size = base_size * 0.9),
      axis.line         = element_line(colour = "black", linewidth = 0.4),
      axis.ticks        = element_line(colour = "black", linewidth = 0.4),
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size * 0.9),
      legend.key.size   = unit(0.55, "cm"),
      legend.key.width  = unit(1.2, "cm"),
      legend.background = element_rect(fill = "white", colour = NA),
      panel.border      = element_blank(),
      plot.margin       = margin(4, 6, 4, 4, "mm")
    )
}

# ── データ読み込み ────────────────────────────────────────────
# CSVから読む場合:
# wi_raw      <- read.csv("WI_with_KurodeDamObservation.csv")
# wi_raw$year <- seq(1965, 1965 + nrow(wi_raw) - 1)
# plt2        <- read.csv("plt2.csv")

# パッケージオブジェクトを使う場合:
wi_raw       <- as.data.frame(WI_with_KurodeDamObservation)
wi_raw$year  <- seq(1965, 1965 + nrow(wi_raw) - 1)

# ── プロット対応テーブル ──────────────────────────────────────
plot_meta <- data.frame(
  col   = c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi"),
  label = c("Temperate plot","Ecotone plot",
            "Subarctic plot","Timberline plot"),
  color = c("#E69F00","#9400D3","#00AACC","#555555"),
  lty   = c("solid","dashed","dotdash","dotted"),
  stringsAsFactors = FALSE
)

# ── Wide → Long 変換 ─────────────────────────────────────────
wi_long <- wi_raw |>
  select(year, all_of(plot_meta$col)) |>
  pivot_longer(cols = -year,
               names_to  = "col",
               values_to = "WI") |>
  left_join(plot_meta, by = "col") |>
  mutate(label = factor(label, levels = plot_meta$label))

# ── 各調査年のWI値（番号付き点用） ───────────────────────────
yr_cols <- paste0("yr", 1:7)

survey_pts <- bind_rows(lapply(1:nrow(plt2), function(i) {
  row   <- plt2[i, ]
  pname <- plot_meta$label[plot_meta$col == row$na]
  if (length(pname) == 0) return(NULL)
  years       <- as.integer(row[yr_cols])
  years_plot  <- ifelse(years > 2023, 2023, years)   # 2024はWIなし→2023で代替
  data.frame(col = row$na, label = pname,
             year = years_plot, period = 1:7,
             stringsAsFactors = FALSE)
})) |>
  left_join(wi_long |> select(col, year, WI, color),
            by = c("col", "year")) |>
  mutate(label = factor(label, levels = plot_meta$label))

# ── 植生帯ラベル用データ ─────────────────────────────────────
# 境界線の中央に配置: Temperate=(55+80)/2=67.5, Ecotone=50, Subarctic=(10+45)/2=27.5
zone_df <- data.frame(
  y     = #c(67.5, 50.0, 27.5),
         c(72, 47.0,33),
  label = c("Temperate zone", "Ecotone zone", "Subarctic zone")
)

# ── カラー・線種スケール ──────────────────────────────────────
col_vals <- setNames(plot_meta$color, plot_meta$label)
lty_vals <- setNames(plot_meta$lty,   plot_meta$label)

# ── プロット ──────────────────────────────────────────────────
fig4 <- ggplot() +

  # Kira (1948) 植生帯境界線
  geom_hline(yintercept = c(45, 55),
             linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +

  # 植生帯ラベル（16pt・斜体・境界線の間の中央）
  geom_text(data = zone_df,
            aes(x = 1963, y = y, label = label),
            hjust = 0, vjust = 0.5,
            size  = 16 / .pt,          # ptをggplot2単位に変換
            colour = "grey60",
            fontface = "italic") +

  # WI経年変化（折れ線）
  geom_line(data = wi_long,
            aes(x = year, y = WI,
                colour   = label,
                linetype = label),
            linewidth = 1.4, alpha = 0.85) +

  # 調査期間の白抜き丸
  geom_point(data = survey_pts,
             aes(x = year, y = WI, colour = label),
             shape = 21, fill = "white",
             size = 5, stroke = 1.0) +

  # 調査期間番号
  geom_text(data = survey_pts,
            aes(x = year, y = WI,
                label  = period,
                colour = label),
            size = 3.2, fontface = "bold",show.legend = FALSE) +

  # スケール
  scale_colour_manual(values = col_vals) +
  scale_linetype_manual(values = lty_vals) +
  scale_x_continuous(
    breaks = seq(1965, 2025, by = 5),
    limits = c(1963, 2026),
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  scale_y_continuous(
    breaks = seq(10, 80, by = 10),
    limits = c(10, 80),
    expand = expansion(mult = c(0.02, 0.02))
  ) +

  # 軸ラベル
  labs(x = "Year",
       y = "Warmth index (\u00b0C\u00b7month)") +

  # テーマ
  theme_tateyama(base_size = 14) +
  theme(
    legend.position      = c(0.98, 0.02),
    legend.justification = c(1, 0),
    axis.text.x          = element_text(angle = 45, hjust = 1),
    legend.key.width     = unit(2.5, "cm")  # ← 追加
  )

print(fig4)

# ── 保存（査読中：PNG） ───────────────────────────────────────
# ggsave("Fig04_warmth_index.png",
#        plot   = fig4,
#        width  = 174, height = 150, units = "mm",
#        dpi    = 300, bg = "white")

# ── 保存（受理後：PDF、Ubuntu推奨） ──────────────────────────
# ggsave("Fig04_warmth_index.pdf",
#        plot   = fig4,
#        width  = 174, height = 150, units = "mm",
#        device = cairo_pdf)
