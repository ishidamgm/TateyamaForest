#' Fig_wi_ba_cor_JVS2
#'
#' @returns
#' @export
#'
#' @examples
#' #'
#' # library(ggplot2)
#' # library(patchwork)
#'
#' Fig_wi_ba_cor_JVS2()
#'
Fig_wi_ba_cor_JVS2<-function(){
  wi.<-subset(TemperatureWIAbiesPopulation,plot=="Kaminokodaira")$WI
  dz<-.data_Fig_yr_ba_kaminokodaira_zone_2024
  dsp<-.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024
  #'
  sp_list    <- unique(dsp$sp)
  group_list <- unique(dz$sp)

  make_plot <- function(data_sub, x_vec, label_main) {
    ba.    <- data_sub$ba_ratio
    ba_mid <- (ba.[-length(ba.)] + ba.[-1]) / 2
    df     <- data.frame(wi = x_vec, ba = ba_mid)

    res   <- lm(ba ~ wi, data = df)
    cf    <- coef(res)
    sm    <- summary(res)
    r_val <- sqrt(sm$r.squared) * sign(cf[2])
    p_val <- sm$coefficients[2, 4]
    p_lab <- ifelse(p_val < 0.001, "p < 0.001", sprintf("p = %.3f", p_val))
    sub_lab <- sprintf("y = %.4f x %+.4f,  r = %.3f,  %s",
                       cf[2], cf[1], r_val, p_lab)

    ggplot(df, aes(x = wi, y = ba)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.7) +
      labs(title = label_main,
           subtitle = sub_lab,
           x = "WI (interval mean)",
           y = "BA ratio (midpoint)") +
      theme_classic(base_size = 11) +
      theme(
        plot.title    = element_text(face = "italic", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 8.5),
        axis.title    = element_text(size = 10),
        axis.text     = element_text(size = 9)
      )
  }

  # 上段(a): 種別
  plots_a <- lapply(1:3, function(i) {
    d_sub <- subset(dz, sp== group_list[i])  # 列名要確認
    make_plot(d_sub, wi., as.character(group_list[i]))

  })

  # 下段(b): 樹種グループ別
  plots_b <- lapply(1:3, function(i) {
    d_sub <- subset(dsp, sp == sp_list[i])
    make_plot(d_sub, wi., as.character(sp_list[i]))

  })

  # patchworkで結合、(a)(b)タグ付与
  wrap_plots(plots_a, nrow = 1) /
    wrap_plots(plots_b, nrow = 1) +
    plot_annotation(tag_levels = list(c("(a)", "", "", "(b)", "", "")))
}
