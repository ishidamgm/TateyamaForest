#' Fig_wi_ba_cor_ancova_JVS2
#'
#' @param clim_var
#'
#' @returns
#' @export
#'
#' @examples
#'
#' res<-Fig_wi_ba_cor_ancova_JVS2()
#' res
Fig_wi_ba_cor_ancova_JVS2 <- function(clim_var = "WI") {
  d_clim <- subset(TemperatureWIAbiesPopulation, plot == "Kaminokodaira")
  wi.    <- d_clim[[clim_var]]
  dz     <- .data_Fig_yr_ba_kaminokodaira_zone_2024
  dsp    <- .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024

  group_list <- unique(dz$sp)
  sp_list    <- unique(dsp$sp)

  # 区間中点データ生成
  to_interval <- function(df) {
    sp_list_ <- unique(as.character(df$sp))
    do.call(rbind, lapply(sp_list_, function(s) {
      d.     <- subset(df, as.character(sp) == s)
      ba.    <- d.$ba_ratio
      ba_mid <- (ba.[-length(ba.)] + ba.[-1]) / 2
      data.frame(sp = s, WI = wi., ba_ratio = ba_mid,
                 stringsAsFactors = FALSE)
    }))
  }

  f_zone    <- to_interval(dz)
  f_species <- to_interval(dsp)

  # ANCOVA：slopesとpairwise結果をデータフレームで返す
  run_ancova <- function(data, label) {
    data$sp <- factor(data$sp)
    m        <- lm(ba_ratio ~ WI * sp, data = data)
    b        <- coef(m)
    sp_levels <- levels(data$sp)

    # slopes
    slopes_df <- data.frame(
      label    = label,
      sp       = sp_levels,
      slope    = sapply(sp_levels, function(lv) {
        int_nm <- paste0("WI:sp", lv)
        unname(b["WI"]) + ifelse(int_nm %in% names(b), unname(b[int_nm]), 0)
      }),
      stringsAsFactors = FALSE
    )

    # pairwise
    pw_df <- run_pairwise(data, "ba_ratio", "WI", "sp")
    pw_df$label <- label

    list(slopes = slopes_df, pairwise = pw_df)
  }

  res_zone    <- run_ancova(f_zone,    "By zone")
  res_species <- run_ancova(f_species, "By species")

  slopes_all   <- rbind(res_zone$slopes,   res_species$slopes)
  pairwise_all <- rbind(res_zone$pairwise, res_species$pairwise)

  # 作図
  make_plot <- function(data_sub, x_vec, label_main) {
    ba.    <- data_sub$ba_ratio
    ba_mid <- (ba.[-length(ba.)] + ba.[-1]) / 2
    df     <- data.frame(wi = x_vec, ba = ba_mid)
    res    <- lm(ba ~ wi, data = df)
    cf     <- coef(res)
    sm     <- summary(res)
    r_val  <- sqrt(sm$r.squared) * sign(cf[2])
    p_val  <- sm$coefficients[2, 4]
    #p_lab  <- ifelse(p_val < 0.001, "p < 0.001", sprintf("p = %.3f", p_val))
    # sub_lab <- sprintf("y = %.4f x %+.4f,  r = %.3f,  %s",
    #                    cf[2], cf[1], r_val, p_lab)
    p_lab <- ifelse(p_val < 0.001, "p<0.001",
                    ifelse(p_val < 0.01,  sprintf("p=%.3f", p_val),
                           sprintf("p=%.3f", p_val)))
    sub_lab <- sprintf("y=%.3fx%+.3f, r=%.3f, %s",
                       cf[2], cf[1], r_val, p_lab)

    ggplot(df, aes(x = wi, y = ba)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE,
                  color = "black", linewidth = 0.7) +
      labs(title    = label_main,
           subtitle = sub_lab,
           x        = sprintf("%s (interval mean)", clim_var),
           y        = "BA ratio (midpoint)") +
      theme_classic(base_size = 11) +
      theme(
        plot.title    = element_text(face = "italic", hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 8.5),
        axis.title    = element_text(size = 10),
        axis.text     = element_text(size = 9)
      )
  }

  plots_a <- lapply(1:3, function(i)
    make_plot(subset(dz,  sp == group_list[i]), wi., as.character(group_list[i])))
  plots_b <- lapply(1:3, function(i)
    make_plot(subset(dsp, sp == sp_list[i]),    wi., as.character(sp_list[i])))

  fig <- wrap_plots(plots_a, nrow = 1) /
    wrap_plots(plots_b, nrow = 1) +
    plot_annotation(tag_levels = list(c("(a)", "", "", "(b)", "", "")))

  #print(fig)
  suppressMessages(print(fig))

  invisible(list(
    slopes   = slopes_all,
    pairwise = pairwise_all
  ))
}
