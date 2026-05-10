#' Fig_wi_ba_cor_ancova_JVS2
#'
#' @param clim_var
#'
#' @returns
#' @export
#'
#' @examples
#' res<-Fig_wi_ba_cor_ancova_JVS2("WI")
#' res
Fig_wi_ba_cor_ancova_JVS2 <- function() {


  dz    <- data_Fig_yr_ba_kaminokodaira_zone_2024
  dsp <- data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024



  group_list <- unique(dz$sp)
  sp_list    <- unique(dsp$sp)
  wi.    <- subset(dz,sp==group_list[1])$WI

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

  res_zone    <- run_ancova(dz,    "By zone")
  res_species <- run_ancova(dsp, "By species")

  slopes_all   <- rbind(res_zone$slopes,   res_species$slopes)
  pairwise_all <- rbind(res_zone$pairwise, res_species$pairwise)

  # 作図
  make_plot <- function(data_sub, x_vec, label_main) {
    ba.    <- data_sub$ba_ratio
    #ba_mid <- (ba.[-length(ba.)] + ba.[-1]) / 2
    df     <- data.frame(wi = x_vec, ba = ba.)
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
           x        = sprintf("%s ", clim_var),
           y        = "BA ratio (stand-based)") +
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
    make_plot(subset(dsp, sp == sp_list[i]),   wi., as.character(sp_list[i])))

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





#' Fig_Abies_wi_ba_mortality
#'
#' @returns
#' @export
#'
#' @examples
#' res<-Fig_Abies_wi_ba_mortality()
#' res
Fig_Abies_wi_ba_mortality<-function(){
  . <- TemperatureWIAbies_Population_BA_Mortality
  plot. <- c("Kagamiishi", "Matsuotoge","Kaminokodaira" )
  plot_labels <- c( "Timberline plot", "Subarctic plot","Ecotone plot")
  .$plot <- factor(.$plot, levels = plot., labels = plot_labels)


  # p値をシンボルに変換
  # p_to_sym <- function(p) {
  #   ifelse(p < 0.001, "***",
  #          ifelse(p < 0.01,  "**",
  #                 ifelse(p < 0.05,  "*",
  #                        ifelse(p < 0.1,   "·", "ns"))))
  # }
  p_to_sym <- function(p=0.02) {#  p_to_sym(0.02)
    sym <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "·", "ns"))))
    p_str <- ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    #sprintf("%s(%s)", sym, p_str)
    sprintf("%s%s", p_str, sym)
  }
  # ANCOVA pairwise ####
  get_matrix <- function(var="BAratio") { # get_matrix("BAratio")
    df <- data.frame(
      plot     = .$plot,
      WI       = .$WI,
      response = .[[var]]
    )
    names(df)[3] <- var
    pw <- run_pairwise(df, var, "WI", "plot")
    lvs <- levels(.$plot)
    mat <- matrix("", nrow = 3, ncol = 3,
                  dimnames = list(lvs, lvs))
    for (k in seq_len(nrow(pw))) {
      # "A vs B" を分割
      parts <- strsplit(pw$comparison[k], " vs ")[[1]]
      r <- trimws(parts[1])
      o <- trimws(parts[2])
      sym <- p_to_sym(pw$p_value[k])
      mat[r, o] <- sym
      mat[o, r] <- sym
    }
    diag(mat) <- "—"
    return(list(pw=pw,mat=mat))
  }

  BAratio_ancova <- get_matrix("BAratio")
  mortality_ancova <- get_matrix("mortality")

  mat_ba <- BAratio_ancova$mat
  mat_mt <-mortality_ancova$mat

  # 上三角をBA、下三角をMortality
  combined_mat <- mat_ba
  combined_mat[lower.tri(combined_mat)] <- mat_mt[lower.tri(mat_mt)]


  # 共通theme
  theme_jvs <- theme_classic(base_size = 11) +
    theme(
      axis.title = element_text(size = 10),
      axis.text  = element_text(size = 9),
      legend.title = element_blank()
    )

  col_vals <- c("Ecotone plot"   = "black",
                "Subarctic plot" = "black",
                "Timberline plot" = "black")


  # 回帰統計＋plot名を計算する関数 ####
  # get_reg_labels <- function(var) {
  #   do.call(rbind, lapply(levels(.$plot), function(pl) {
  #     d.    <- subset(., plot == pl)
  #     res   <- lm(d.[[var]] ~ d.$WI)
  #     cf    <- coef(res)
  #     sm    <- summary(res)
  #     r_val <- sqrt(sm$r.squared) * sign(cf[2])
  #     p_val <- sm$coefficients[2, 4]
  #     p_lab <- ifelse(p_val < 0.001, "p<0.001",
  #                     sprintf("p=%.3f", p_val))
  #     data.frame(
  #       plot      = pl,
  #       reg_label = sprintf("y=%.3fx%+.3f, r=%.3f, %s",
  #                           cf[2], cf[1], r_val, p_lab),
  #       stringsAsFactors = FALSE
  #     )
  #   }))
  # }
  get_reg_labels <- function(var) {
    do.call(rbind, lapply(levels(.$plot), function(pl) {
      d.    <- subset(., plot == pl)
      res   <- lm(as.formula(paste(var, "~ WI")), data = d.)
      cf    <- coef(res)
      sm    <- summary(res)
      r_val <- sqrt(sm$r.squared) * sign(cf[2])
      p_val <- sm$coefficients[2, 4]
      p_lab <- ifelse(p_val < 0.001, "p<0.001",
                      sprintf("p=%.3f", p_val))
      data.frame(
        plot      = pl,
        reg_label = sprintf("y=%.3fx%+.3f, r=%.3f, %s",
                            cf[2], cf[1], r_val, p_lab),
        stringsAsFactors = FALSE
      )
    }))
  }
  reg_ba <- get_reg_labels("BAratio")
  reg_mt <- get_reg_labels("mortality")

  # 位置情報（plot名＋回帰式の中央x）####
  pos_ba <- data.frame(
    plot      = levels(.$plot),
    x         = c(25.5, 38.5, 53.0),
    y_name    = c(1.25, 1.08, 1.04)+0.013,  # plot名のy
    y_reg     = c(1.22, 1.05, 1.01)+0.013   # 回帰式のy
  )
  pos_mt <- data.frame(
    plot      = levels(.$plot),
    x         = c(25.5, 38.5, 53.5),
    y_name    = c(0.17, 0.19, 0.40)+0.013,
    y_reg     = c(0.14, 0.16, 0.37)+0.013
  )


  theme_jvs <- theme_classic(base_size = 11) +
    theme(
      axis.title   = element_text(size = 10),
      axis.text    = element_text(size = 9),
      legend.title = element_blank()
    )


  # 1. 回帰統計の計算
  reg_ba <- get_reg_labels("BAratio")
  reg_mt <- get_reg_labels("mortality")

  # 2. 位置情報の定義
  pos_ba <- data.frame(
    plot   = levels(.$plot),
    x      = c(25.5, 38.5, 53.0),
    y_name = c(1.25, 1.12, 1.08),
    y_reg  = c(1.21, 1.08, 1.04)
  )
  pos_mt <- data.frame(
    plot   = levels(.$plot),
    x      = c(25.5, 38.5, 53.5),
    y_name = c(0.20, 0.22, 0.44),
    y_reg  = c(0.16, 0.18, 0.40)
  )

  # 3. mergeで位置情報を結合
  reg_ba <- merge(reg_ba, pos_ba, by = "plot")
  reg_mt <- merge(reg_mt, pos_mt, by = "plot")

  # (a) BA ratio の作図　  p_ba　####
  p_ba <- ggplot(., aes(x = WI, y = BAratio,
                        color = plot, group = plot)) +
    geom_point(size = 1.5, shape = 16) +
    geom_line(linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    scale_color_manual(values = col_vals) +
    labs(x = "WI", y = "BA ratio ((species-based)") +
    coord_cartesian(xlim = c(20, 57), ylim = c(0.78, 1.30)) +
    # plot名（bold）
    geom_text(data = reg_ba,
              aes(x = x, y = y_name, label = plot, color = plot),
              inherit.aes = FALSE, size = 2.8,
              fontface = "bold", hjust = 0.5,
              family = "sans") +
    # 回帰式（center）
    geom_text(data = reg_ba,
              aes(x = x, y = y_reg, label = reg_label, color = plot),
              inherit.aes = FALSE, size = 2.2,
              hjust = 0.5, family = "sans") +
    theme_jvs +
    theme(legend.position = "none")



  # p_mt Mortality の作図 ####
  ## 星取り表をgeom_text用data.frameに変換 ####
  lvs_short <- c( "Tim", "Sub","Eco")

  x_pos <- seq(25,32,length=3)#c(22.5, 24.5, 26.5)
  y_pos <- seq(0.43,0.35,length=3)#c(1.18, 1.14, 1.10)

  mat_df <- data.frame(
    x     = rep(x_pos, times = 3),
    y     = rep(y_pos, each  = 3),
    label = as.vector(t(combined_mat))
  )

  p_mt <- ggplot(., aes(x = WI, y = mortality,
                        color = plot, group = plot)) +
    geom_point(size = 1.5, shape = 16) +
    geom_line(linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    scale_color_manual(values = col_vals) +
    labs(x = "WI", y = "Cumulative Mortality ratio") +
    coord_cartesian(xlim = c(20, 57), ylim = c(0, 0.55)) +
    # plot名（bold）####
  geom_text(data = reg_mt,
            aes(x = x, y = y_name, label = plot, color = plot),
            inherit.aes = FALSE, size = 2.8,
            fontface = "bold", hjust = 0.5,
            family = "sans") +
    # 回帰式（center）####
  geom_text(data = reg_mt,
            aes(x = x, y = y_reg, label = reg_label, color = plot),
            inherit.aes = FALSE, size = 2.5,
            hjust = 0.5, family = "sans") +
    # 星取り表（既存）####


  annotate("text", x = x_pos, y = 0.46,
           label = lvs_short, size = 2.5, fontface = "bold") +
    annotate("text", x = 23.0, y = y_pos,
             label = lvs_short, size = 2.5, fontface = "bold") +
    geom_text(data = mat_df, aes(x = x, y = y, label = label),
              inherit.aes = FALSE, size = 2.8, family = "mono") +
    annotate("text", x = 23.0, y = 0.55,
             label = "Slope comparisons by ANCOVA (P values)\nupper: BA  lower: Mortality",
             hjust = 0, vjust = 1, size = 2.2, color = "grey40") +
    theme_jvs +
    theme(legend.position = "none")  # 凡例を削除

  suppressMessages(
    print(
      p_ba / p_mt +
        plot_annotation(tag_levels = list(c("(a)", "(b)")))
    )
  )

  invisible(list( regression_ba=reg_ba,regression_mortality=reg_mt,BAratio_ancova=BAratio_ancova,mortality_ancova=mortality_ancova))
}
