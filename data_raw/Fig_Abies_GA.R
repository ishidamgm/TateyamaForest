library(patchwork)

p1 <- Fig_Abies_GA(var = "BAratio",  save_path = NULL)
p2 <- Fig_Abies_GA(var = "mortality", save_path = NULL)
combined <- p1 / p2
combined

#' Fig_Abies_GA
#'
#' Graphical Abstract用のシンプル版プロット。
#' 回帰式・p値・ANCOVA表などの統計詳細は一切表示せず、
#' 3プロット(Timberline/Subarctic/Ecotone)の傾向線と凡例のみを示す。
#' 50mm x 60mm程度に縮小しても判読できるよう、文字・線を大きめに設定。
#'
#' @param var 縦軸に使う変数名。デフォルトは "BAratio"。
#'            本文Fig.との対応上、mortalityにも切替可能。
#' @param save_path 保存先パス(NULLなら保存せず画面表示のみ)
#' @param width_mm 出力画像の幅(mm)。デフォルト60mm。
#' @param height_mm 出力画像の高さ(mm)。デフォルト50mm。
#' @param dpi 出力解像度。デフォルト300(140dpi以上の指定を余裕を持ってクリア)。
#'
#' @returns ggplotオブジェクト(invisible)
#' @export
#'
#' @examples
#' res <- Fig_Abies_GA()
#' res <- Fig_Abies_GA(save_path = "graphical_abstract.png")
Fig_Abies_GA <- function(var = "BAratio",
                         save_path = NULL,
                         width_mm = 60,
                         height_mm = 50,
                         dpi = 300) {

  . <- Abies
  plot. <- c("Kagamiishi", "Matsuotoge", "Kaminokodaira")
  plot_labels <- c("Timberline plot", "Subarctic plot", "Ecotone plot")
  .$plot <- factor(.$plot, levels = plot., labels = plot_labels)

  y_lab <- if (var == "BAratio") "Basal Area ratio (Abies mariesii)" else "Cumulative mortality ratio"

  # プロットごとに色分け(グレースケール推奨。カラー版が必要ならcol_valsを変更)
  col_vals <- c("Timberline plot" = "#1b9e77",
                "Subarctic plot"  = "#7570b3",
                "Ecotone plot"    = "#d95f02")

  # 凡例ラベル位置(各プロットの右端付近に直接ラベルを置き、legendは非表示にする方が
  # 縮小時の視認性が高い。x位置はWIレンジに応じて調整。)
  label_pos <- data.frame(
    plot = levels(.$plot),
    x    = c(34.5, 43.5, 63.5),
    y    = sapply(levels(.$plot), function(pl) {
      d <- subset(., plot == pl)
      fit <- lm(as.formula(paste(var, "~ WI")), data = d)
      max(predict(fit))
    })
  )

  p <- ggplot(., aes(x = WI, y = .data[[var]], color = plot, group = plot)) +
    geom_point(size = 2.2, alpha = 0.85) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.6) +
    scale_color_manual(values = col_vals) +
    labs(x = "Warmth Index", y = y_lab) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 20) +
    theme(
      legend.position   = "none",
      axis.title        = element_text(size = 20, face = "bold"),
      axis.text         = element_text(size = 16),
      axis.line         = element_line(linewidth = 0.8),
      axis.ticks        = element_line(linewidth = 0.8),
      plot.margin       = margin(5, 30, 5, 5)
    ) +
    geom_text(data = label_pos,
              aes(x = x, y = y, label = plot, color = plot),
              inherit.aes = FALSE, fontface = "bold",
              size = 5, hjust = 0)

  print(p)

  if (!is.null(save_path)) {
    ggsave(save_path, plot = p,
           width = width_mm, height = height_mm,
           units = "mm", dpi = dpi, bg = "white")
    message(sprintf("Saved: %s (%gmm x %gmm, %g dpi)", save_path, width_mm, height_mm, dpi))
  }

  invisible(p)
}
