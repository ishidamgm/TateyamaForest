# _________________________________________________________
# Abies mariesii 累積死亡率・BA比 vs. WI / year の解析
# 1. Pearson相関（WI vs. 累積死亡率）
# 2. ANCOVA（累積死亡率 ~ year * plot）傾きのペアワイズ比較
# 3. Pearson相関（WI vs. BA比）
# 4. ANCOVA（BA比 ~ WI * 樹種/zone）傾きのペアワイズ比較
# __________________________________________________________

# setwd("data_raw/JVS2/ANCOVA")
# mortality_ba_ancova.R

#' Statistics_pairwise_slope
#'
#' @param data
#' @param response
#' @param covariate
#' @param groupvar
#' @param ref
#' @param other
#'
#' @returns
#' @export
#'
#'
#' @examples
#'
pairwise_slope <- function(data, response, covariate, groupvar, ref, other) {
  data$grp <- factor(data[[groupvar]],
                     levels = c(ref, setdiff(unique(data[[groupvar]]), ref)))
  fml  <- as.formula(paste(response, "~", covariate, "* grp"))
  m    <- lm(fml, data = data)
  cf   <- summary(m)$coefficients
  row  <- paste0(covariate, ":grp", other)
  b    <- coef(m)
  s_ref   <- unname(b[covariate])
  s_other <- unname(b[covariate]) + unname(b[row])
  c(slope_ref   = s_ref,
    slope_other = s_other,
    estimate    = unname(cf[row, "Estimate"]),
    t           = unname(cf[row, "t value"]),
    p           = unname(cf[row, "Pr(>|t|)"]))
}

#' pairwise for ANCOVA
#'
#' @param data
#' @param response
#' @param covariate
#' @param groupvar
#' @param bonf
#'
#' @returns
#' @export
#'
#' @examples
#'
run_pairwise <- function(data, response, covariate, groupvar, bonf = TRUE) {
  groups <- levels(factor(data[[groupvar]]))
  pairs  <- combn(groups, 2, simplify = FALSE)
  cat(sprintf("\n--- Pairwise slope comparisons (%s ~ %s) ---\n", response, covariate))
  results <- data.frame()
  for (pair in pairs) {
    res    <- pairwise_slope(data, response, covariate, groupvar, pair[1], pair[2])
    p_bonf <- min(res["p"] * length(pairs), 1)
    cat(sprintf("%s vs. %s:\n", pair[1], pair[2]))
    cat(sprintf("  slope(%s)=%.5f, slope(%s)=%.5f, diff=%.5f, t=%.3f, p=%.4f%s\n",
                pair[1], res["slope_ref"],
                pair[2], res["slope_other"],
                res["estimate"], res["t"], res["p"],
                if (bonf) sprintf(", p_Bonf=%.4f", p_bonf) else ""))
    results <- rbind(results, data.frame(
      comparison   = paste(pair[1], "vs", pair[2]),
      slope_ref    = round(res["slope_ref"],   5),
      slope_other  = round(res["slope_other"], 5),
      slope_diff   = round(res["estimate"],    5),
      t_value      = round(res["t"],           3),
      p_value      = round(res["p"],           4),
      p_Bonferroni = round(p_bonf,             4)
    ))
  }
  invisible(results)
}

# データ構成 #####
#load("Abies_death_ratio.RData")
# wi  <- read.csv("wi2024.csv")
# names(wi)[1] <- "year"

wi  <- wi_year # Kume et al.
f1  <- .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024
f2  <- .data_Fig_yr_ba_kaminokodaira_zone_2024
# f1  <- readRDS("Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024.rds")
# f2  <- readRDS("Fig_yr_ba_kaminokodaira_zone_2024.rds")
# .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024 <- f1
# save( .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
# file=" .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024.RData")
# .data_Fig_yr_ba_kaminokodaira_zone_2024<- f2
# save( .data_Fig_yr_ba_kaminokodaira_zone_2024,
#       file=" .data_Fig_yr_ba_kaminokodaira_zone_2024.RData")


## Kaminokodaira の調査年WI ################
kami_years <- sort(unique(f1$year))
wi_kami    <- data.frame(
  year = kami_years,
  WI   = wi[match(kami_years, wi$year), "Kaminokodaira"]
)

## BA データにWIを結合 ####
f1$WI <- wi_kami$WI[match(f1$year, wi_kami$year)]
f2$WI <- wi_kami$WI[match(f2$year, wi_kami$year)]

## 死亡率データにWIを結合 ####
get_death_data <- function(plot) {
  df      <- Abies_death_ratio[[plot]]
  df$WI   <- wi[match(df$year, wi$year), plot]
  df$plot <- plot
  df
}

.data_Abies_wi_deathRatio <- rbind(
  get_death_data("Kaminokodaira"),
  get_death_data("Matsuotoge"),
  get_death_data("Kagamiishi")
)
.data_Abies_wi_deathRatio$plot <- factor(.data_Abies_wi_deathRatio$plot,
  levels = c("Kaminokodaira", "Matsuotoge", "Kagamiishi"))

f1$sp <- factor(f1$sp, levels = c("Fagus crenata", "Cryptomeria japonica", "Abies mariesii"))
f2$sp <- factor(f2$sp, levels = c("Temperate tree species", "Ecotone tree species", "Subarctic tree species"))

# save(.data_Abies_wi_deathRatio,file=".data_Abies_wi_deathRatio.RData")

# >>>>>>>>>>>>>> ####
# 1. Pearson相関: WI vs. Abies 累積死亡率（各プロット n=7, プールn=21）####

#' 1. Pearson相関: WI vs. Abies 累積死亡率（各プロット n=7, プールn=21）####
#'
#' @returns
#' @export
#'
#' @examples
#' statistics_cor_WI_AbiesCumulativeMortality()
#'
statistics_cor_WI_AbiesCumulativeMortality<-function(){
  message("Pearson correlation: WI vs. cumulative mortality")

  d.<-c()
  for (plot in levels(.data_Abies_wi_deathRatio$plot)) {
    df <- .data_Abies_wi_deathRatio[.data_Abies_wi_deathRatio$plot == plot, ]
    ct <- cor.test(df$WI, df$death_ratio, method = "pearson")
    #cat(sprintf("%-20s r = %6.3f, p = %.4f (n=7)\n", plot, ct$estimate, ct$p.value))
    d.<-rbind(d.,data.frame(plot=plot, r=ct$estimate,p=ct$p.value,n=7))
  }
  ct_all <- cor.test(.data_Abies_wi_deathRatio$WI, .data_Abies_wi_deathRatio$death_ratio, method = "pearson")
  #cat(sprintf("%-20s r = %6.3f, p = %.4f (n=21)\n", "Pooled", ct_all$estimate, ct_all$p.value))
  cor.WI.AbiesMortality<-rbind(d.,data.frame(plot="Pooled", r=ct_all$estimate,p=ct_all$p.value,n=21))
  return(cor.WI.AbiesMortality)
}



# 2. ANCOVA: 累積死亡率 ~ year * plot ####


#' 2. ANCOVA: 累積死亡率 ~ year * plot
#'
#' @returns
#' @export
#'
#' @examples
#' statistics_ANCOVA_Abies_CumurativeDeathRatio_year_plot()
#'
statistics_ANCOVA_Abies_CumurativeDeathRatio_year_plot<-function(){
  m_death <- lm(death_ratio ~ year * plot, data = .data_Abies_wi_deathRatio)
  print(summary(m_death))

  # 各プロットの傾き
  b <- coef(m_death)
  slopes_death <- c(
    Kaminokodaira = b["year"],
    Matsuotoge    = b["year"] + b["year:plotMatsuotoge"],
    Kagamiishi    = b["year"] + b["year:plotKagamiishi"]
  )
  cat("--- Slope of each plot ---\n")
  for (nm in names(slopes_death))
    cat(sprintf("  %-20s %.6f /yr\n", nm, slopes_death[nm]))

  run_pairwise(.data_Abies_wi_deathRatio, "death_ratio", "year", "plot")
}


#____________________________________________________________
# if(0){
#   cat("\n============================================================\n")
#   cat("2. ANCOVA: cumulative mortality ~ year * plot\n")
#   cat("============================================================\n")
#   m_death <- lm(death_ratio ~ year * plot, data = .data_Abies_wi_deathRatio)
#   print(summary(m_death))
#
#   # 各プロットの傾き
#   b <- coef(m_death)
#   slopes_death <- c(
#     Kaminokodaira = b["year"],
#     Matsuotoge    = b["year"] + b["year:plotMatsuotoge"],
#     Kagamiishi    = b["year"] + b["year:plotKagamiishi"]
#   )
#   cat("--- Slope of each plot ---\n")
#   for (nm in names(slopes_death))
#     cat(sprintf("  %-20s %.6f /yr\n", nm, slopes_death[nm]))
#
#   run_pairwise(.data_Abies_wi_deathRatio, "death_ratio", "year", "plot")
# }
#____________________________________________________________


# 3. Pearson相関: WI vs. BA比（樹種別・zone別）####


#' 3. Pearson相関: WI vs. BA比（樹種別・zone別）
#'
#' @returns
#' @export
#'
#' @examples
#' statistics_cor_wi_ba_EcotonePlot()
#'
statistics_cor_wi_ba_EcotonePlot<-function(){
  cat("\n============================================================\n")
  cat("3. Pearson correlation: WI vs. BA ratio (Kaminokodaira, n=7)\n")
  cat("============================================================\n")

  cat("\n--- By species (f1) ---\n")
  for (sp in levels(f1$sp)) {
    df <- f1[f1$sp == sp, ]
    ct <- cor.test(df$WI, df$ba_ratio, method = "pearson")
    cat(sprintf("%-25s r = %6.3f, p = %.4f\n", sp, ct$estimate, ct$p.value))
  }

  cat("\n--- By zone (f2) ---\n")
  for (sp in levels(f2$sp)) {
    df <- f2[f2$sp == sp, ]
    ct <- cor.test(df$WI, df$ba_ratio, method = "pearson")
    cat(sprintf("%-28s r = %6.3f, p = %.4f\n", sp, ct$estimate, ct$p.value))
  }
}


#____________________________________________________________
# if(0){
#   cat("\n============================================================\n")
#   cat("3. Pearson correlation: WI vs. BA ratio (Kaminokodaira, n=7)\n")
#   cat("============================================================\n")
#
#   cat("\n--- By species (f1) ---\n")
#   for (sp in levels(f1$sp)) {
#     df <- f1[f1$sp == sp, ]
#     ct <- cor.test(df$WI, df$ba_ratio, method = "pearson")
#     cat(sprintf("%-25s r = %6.3f, p = %.4f\n", sp, ct$estimate, ct$p.value))
#   }
#
#   cat("\n--- By zone (f2) ---\n")
#   for (sp in levels(f2$sp)) {
#     df <- f2[f2$sp == sp, ]
#     ct <- cor.test(df$WI, df$ba_ratio, method = "pearson")
#     cat(sprintf("%-28s r = %6.3f, p = %.4f\n", sp, ct$estimate, ct$p.value))
#   }
# }
#____________________________________________________________

# 4. ANCOVA: BA比 ~ WI * 樹種/zone  傾きのペアワイズ比較 ####

# 4. ANCOVA: BA比 ~ WI * 樹種/zone  傾きのペアワイズ比較
#' Title
#'
#' @returns
#' @export
#'
#' @examples
#' statistics_ANCOVA_wi_ba_EcotonePlot()
#'
statistics_ANCOVA_wi_ba_EcotonePlot<-function(){
  cat("\n============================================================\n")
  cat("4. ANCOVA: BA ratio ~ WI * species/zone\n")
  cat("============================================================\n")

  cat("\n--- By species (f1) ---\n")
  m_f1 <- lm(ba_ratio ~ WI * sp, data = f1)
  print(summary(m_f1))
  b1 <- coef(m_f1)
  slopes_f1 <- c(
    "Fagus crenata"        = b1["WI"],
    "Cryptomeria japonica" = b1["WI"] + b1["WI:spCryptomeria japonica"],
    "Abies mariesii"       = b1["WI"] + b1["WI:spAbies mariesii"]
  )

  # Table 5  by species ####
  cat("--- Slope of each species ---\n")
  for (nm in names(slopes_f1))
    cat(sprintf("  %-25s %.5f /WI\n", nm, slopes_f1[nm]))

  run_pairwise(f1, "ba_ratio", "WI", "sp")

  ## Table 5  by zone species ####
  cat("\n--- By zone (f2) ---\n")
  m_f2 <- lm(ba_ratio ~ WI * sp, data = f2)
  print(summary(m_f2))
  b2 <- coef(m_f2)
  slopes_f2 <- c(
    "Temperate tree species" = b2["WI"],
    "Ecotone tree species"   = b2["WI"] + b2["WI:spEcotone tree species"],
    "Subarctic tree species" = b2["WI"] + b2["WI:spSubarctic tree species"]
  )

  cat("--- Slope of each zone ---\n")
  for (nm in names(slopes_f2))
    cat(sprintf("  %-28s %.5f /WI\n", nm, slopes_f2[nm]))

  run_pairwise(f2, "ba_ratio", "WI", "sp")

}

#____________________________________________________________

# if(0){
#   cat("\n============================================================\n")
#   cat("4. ANCOVA: BA ratio ~ WI * species/zone\n")
#   cat("============================================================\n")
#
#   cat("\n--- By species (f1) ---\n")
#   m_f1 <- lm(ba_ratio ~ WI * sp, data = f1)
#   print(summary(m_f1))
#   b1 <- coef(m_f1)
#   slopes_f1 <- c(
#     "Fagus crenata"        = b1["WI"],
#     "Cryptomeria japonica" = b1["WI"] + b1["WI:spCryptomeria japonica"],
#     "Abies mariesii"       = b1["WI"] + b1["WI:spAbies mariesii"]
#   )
# }
#
#
#
# #____________________________________________________________
#
# if(0){
# cat("--- Slope of each species ---\n")
# for (nm in names(slopes_f1))
#   cat(sprintf("  %-25s %.5f /WI\n", nm, slopes_f1[nm]))
#
# run_pairwise(f1, "ba_ratio", "WI", "sp")
#
# # cat("\n--- By zone (f2) ---\n")
# m_f2 <- lm(ba_ratio ~ WI * sp, data = f2)
# print(summary(m_f2))
# b2 <- coef(m_f2)
# slopes_f2 <- c(
#   "Temperate tree species" = b2["WI"],
#   "Ecotone tree species"   = b2["WI"] + b2["WI:spEcotone tree species"],
#   "Subarctic tree species" = b2["WI"] + b2["WI:spSubarctic tree species"]
# )
#
# cat("--- Slope of each zone ---\n")
# for (nm in names(slopes_f2))
#   cat(sprintf("  %-28s %.5f /WI\n", nm, slopes_f2[nm]))
#
# run_pairwise(f2, "ba_ratio", "WI", "sp")
#
# cat("\n=== Analysis complete ===\n")
# }
#____________________________________________________________
# <<<<<<<<<<<<<<<<< ####


