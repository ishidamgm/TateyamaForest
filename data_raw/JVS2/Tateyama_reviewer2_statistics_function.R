# Tateyama_reviewer2_statistics.R+
#setwd("~/Dropbox/00D/00/tateyama/TateyamaForest/TateyamaForest/data_raw/JVS2")
wi_raw <- read.csv("WI_with_KurodeDamObservation.csv")
wi_raw$year <- seq(1965, 1965 + nrow(wi_raw) - 1)

library(Kendall)
# WI ####
## 各プロットのWI時系列を用意（例：wi_rawから抽出） ####
plots <- list(
  Temperate  = wi_raw$Bunazaka,
  Ecotone    = wi_raw$Kaminokodaira,
  Subarctic  = wi_raw$Matsuotoge,
  Timberline = wi_raw$Kagamiishi
)

years <- wi_raw$year

for (name in names(plots)) {
  wi <- plots[[name]]

  # Mann-Kendall検定
  mk <- Kendall::MannKendall(wi)

  # 線形回帰（傾き/decade）
  lm_fit <- lm(wi ~ years)
  slope_decade <- coef(lm_fit)[2] * 10

  cat(name, "\n")
  cat("  z =", qnorm(mk$sl/2, lower.tail = FALSE), "\n")
  cat("  p =", mk$sl, "\n")
  cat("  slope =", round(slope_decade, 2), "WI per decade\n\n")
}



#' Mann-Kendall test and lm regression analysis
#'
#' @param yr vector of years
#' @param v  vector of values
#'
#' @returns
#' @export
#'
#' @examples
#' kendall_lm(wi_yaer$year,wi_yaer$Kaminokodaira)
#'
kendall_lm<-function(yr=wi_yaer$year,v=wi_yaer$Kaminokodaira){
  # Mann-Kendall検定
  mk <- Kendall::MannKendall(v)
  # 線形回帰（傾き/decade）
  lm_fit <- lm(wi ~ years)
  return(list(MannKendall=mk,lm_fit=summary(lm_fit)))

}

snow

## Kurobe Damとの関係 ####
kurobe<-read.csv("kurobe_dam_temperature.csv")


## 線形回帰 ####
lm_kurobe <- lm(max ~ year, data = kurobe)
slope_decade <- coef(lm_kurobe)[2] * 10
r2 <- summary(lm_kurobe)$r.squared
pval <- summary(lm_kurobe)$coefficients[2, 4]

cat("slope =", round(slope_decade, 3), "°C/decade\n")
cat("R² =", round(r2, 3), "\n")
cat("p =", pval, "\n")

# 積雪環境 ####
snow_data<-read.csv("snow_data.csv")
unique(snow_data$pl)
plot_map <- data.frame(
  jp    = c("ブナ坂", "上ノ小平", "松尾峠", "鏡石"),
  en    = c("Temperate", "Ecotone", "Subarctic", "Timberline"),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(plot_map)) {
  df <- snow_data[snow_data$pl == plot_map$jp[i], ]

  cat(plot_map$en[i], "\n")

  # 積雪期間（pe）
  pe_clean <- df$pe[!is.na(df$pe)]
  if (length(pe_clean) > 3) {
    mk_pe <- MannKendall(pe_clean)
    cat("  pe: tau =", round(mk_pe$tau, 3),
        "  p =", round(mk_pe$sl, 4), "\n")
  }

  # 積雪深（dep）
  dep_clean <- df$dep[!is.na(df$dep)]
  if (length(dep_clean) > 3) {
    mk_dep <- MannKendall(dep_clean)
    cat("  dep: tau =", round(mk_dep$tau, 3),
        "  p =", round(mk_dep$sl, 4), "\n")
  }
  cat("\n")
}

# wiとオオシラビソ　####

read.csv("dd4_abies.csv")
# プロット対応（Temperateは除外→A. mariesiiの3プロット）
target_plots <- c("Kaminokodaira", "Matsuotoge", "Kagamiishi")

# 各inter-survey intervalのmortality rateと平均WIを収集
results <- data.frame()

for (pl in target_plots) {
  df_pl <- plt2[plt2$na == pl, ]  # 列名要確認

  for (i in 1:6) {  # interval 1-2, 2-3, ..., 6-7
    yr1 <- df_pl[[paste0("yr", i)]]
    yr2 <- df_pl[[paste0("yr", i+1)]]

    # 平均WI
    wi_vals <- wi_raw[wi_raw$year >= yr1 & wi_raw$year <= yr2, pl]
    mean_wi <- mean(wi_vals, na.rm = TRUE)

    # mortality rate（列名要確認）
    mort <- df_pl[[paste0("mort", i)]]  # 列名要確認

    results <- rbind(results, data.frame(
      plot = pl,
      interval = i,
      yr1 = yr1,
      yr2 = yr2,
      mean_wi = mean_wi,
      mortality = mort
    ))
  }
}

print(results)


# stepwise ####
Abies_death_ratio

# 累積死亡率→期間別年率に変換
calc_annual_mort <- function(df) {
  n <- nrow(df)
  result <- data.frame()
  for (i in 2:n) {
    yr1 <- df$year[i-1]
    yr2 <- df$year[i]
    interval <- yr2 - yr1
    # 期間中の死亡率（累積差分）
    delta <- df$death_ratio[i] - df$death_ratio[i-1]
    annual <- delta / interval
    result <- rbind(result, data.frame(
      yr1 = yr1, yr2 = yr2,
      mid_year = (yr1 + yr2) / 2,
      interval = interval,
      annual_mort = annual
    ))
  }
  return(result)
}

mort_Ecotone    <- calc_annual_mort(Abies_death_ratio$Kaminokodaira)
mort_Subarctic  <- calc_annual_mort(Abies_death_ratio$Matsuotoge)
mort_Timberline <- calc_annual_mort(Abies_death_ratio$Kagamiishi)

mort_Ecotone$plot    <- "Ecotone"
mort_Subarctic$plot  <- "Subarctic"
mort_Timberline$plot <- "Timberline"

mort_all <- rbind(mort_Ecotone, mort_Subarctic, mort_Timberline)
print(mort_all)

## ####
# wi_rawの列名確認
colnames(wi_raw)
# プロット対応表
plot_wi_col <- c(
  Ecotone    = "Kaminokodaira",
  Subarctic  = "Matsuotoge",
  Timberline = "Kagamiishi"
)

# 各intervalの平均WIを追加
mort_all$mean_WI <- NA

for (i in 1:nrow(mort_all)) {
  pl   <- mort_all$plot[i]
  yr1  <- mort_all$yr1[i]
  yr2  <- mort_all$yr2[i]
  col  <- plot_wi_col[pl]

  wi_sub <- wi_raw[wi_raw$year >= yr1 & wi_raw$year <= yr2, col]
  mort_all$mean_WI[i] <- mean(wi_sub, na.rm = TRUE)
}

print(mort_all[, c("plot", "yr1", "yr2", "annual_mort", "mean_WI")])

### Pearson相関 ####
cor_test <- cor.test(mort_all$annual_mort, mort_all$mean_WI,
                     method = "pearson")
cat("Pearson r =", round(cor_test$estimate, 3), "\n")
cat("p =", round(cor_test$p.value, 4), "\n\n")

### Spearman相関 ####
cor_sp <- cor.test(mort_all$annual_mort, mort_all$mean_WI,
                   method = "spearman")
cat("Spearman rho =", round(cor_sp$estimate, 3), "\n")
cat("p =", round(cor_sp$p.value, 4), "\n\n")

# プロット平均での相関（3プロット）
plot_means <- aggregate(cbind(annual_mort, mean_WI) ~ plot,
                        data = mort_all, FUN = mean)
print(plot_means)

cor_sp2 <- cor.test(plot_means$annual_mort, plot_means$mean_WI,
                    method = "spearman")
cat("\nPlot-level Spearman rho =", round(cor_sp2$estimate, 3), "\n")
cat("p =", round(cor_sp2$p.value, 4), "\n")


# A.mariesii 死亡率　#####
for (pl in c("Kaminokodaira", "Matsuotoge", "Kagamiishi")) {
  df <- Abies_death_ratio[[pl]]

  # 全期間
  total_mort <- max(df$death_ratio)
  total_yr   <- max(df$year) - min(df$year)
  annual_full <- exp(log(1 - total_mort) / total_yr)

  cat(pl, "\n")
  cat("  全期間 annual survival =", round(annual_full, 4),
      "/ mortality =", round(1 - annual_full, 4), "\n")
}

# Ecotoneの全期間
for (pl in c("Kaminokodaira", "Matsuotoge", "Kagamiishi")) {
  df <- Abies_death_ratio[[pl]]

  # 全期間
  total_mort <- max(df$death_ratio)
  total_yr   <- max(df$year) - min(df$year)
  annual_full <- exp(log(1 - total_mort) / total_yr)

  cat(pl, "\n")
  cat("  全期間 annual survival =", round(annual_full, 4),
      "/ mortality =", round(1 - annual_full, 4), "\n")
}

# Ecotoneの2013-2024も確認
df_eco <- Abies_death_ratio$Kaminokodaira
mort_2013 <- df_eco$death_ratio[df_eco$year == 2013]
mort_2024 <- df_eco$death_ratio[df_eco$year == 2024]
delta <- mort_2024 - mort_2013
annual_recent <- exp(log(1 - delta) / 6)
cat("\nEcotone 2018-2024 annual survival =", round(annual_recent, 4),
    "/ mortality =", round(1 - annual_recent, 4), "\n")

# Ecotoneの2013-2024も確認
df_eco <- Abies_death_ratio$Kaminokodaira
mort_2013 <- df_eco$death_ratio[df_eco$year == 2013]
mort_2024 <- df_eco$death_ratio[df_eco$year == 2024]
delta <- mort_2024 - mort_2013
annual_recent <- exp(log(1 - delta) / 6)
cat("\nEcotone 2013-2024 annual survival =", round(annual_recent, 4),
    "/ mortality =", round(1 - annual_recent, 4), "\n")

# Ecotoneの2018-2024も確認
df_eco <- Abies_death_ratio$Kaminokodaira
mort_2018 <- df_eco$death_ratio[df_eco$year == 2018]
mort_2024 <- df_eco$death_ratio[df_eco$year == 2024]
delta <- mort_2024 - mort_2018
annual_recent <- exp(log(1 - delta) / 6)
cat("\nEcotone 2018-2024 annual survival =", round(annual_recent, 4),
    "/ mortality =", round(1 - annual_recent, 4), "\n")


