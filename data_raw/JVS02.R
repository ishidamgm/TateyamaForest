# JVS02.R

# Table 1  Descriptions of the survey plots. ####

# Fig.1 . Bird’s-eye view of each plot ####

# Fig.2  Photographs of the forest type in each plot ####

# Fig.3  Changes in temperature (daily maximum, daily minimum, and daily average) at the Kurobe Dam ####

# Table 2  Regression analysis of yearly trends in daily temperature at the Kurobe Dam (1965–2023). ####

# Fig.4  Changes in the warmth index for each plot over time ####

# Fig.5  Changes in the maximum snow depth for each plot over time ####

# Fig.6  Changes in the annual snow cover duration (days) for each plot over time ####

# Table 3 Basal areas (BAs) of various tree species in each plot. The BA values represent means during periods 1 to 7. ####

# Fig.7  Warmth index and proportion of tree species in each plot ####

# Fig.8 Warmth index and proportion of tree species in each plot ####

# Fig.9  Changes in the total basal area of the tree species group in the Ecotone plot over time ####

# Fig.10 . Changes of the basal area of major tree species in the Ecotone plot ####

# Fig.11  Distributions of the diameter at breast height of A. mariesii during the first survey in each plot ####

# Fig.12 Changes in the cumulative ratio of dead standing A. mariesii trees in the Ecotone plot in 2000 and the estimated frequency of death  ####

# Fig.13  Cumulative mortality ratio of Abies mariesii over time ####


# Appendix S1####

# Appendix S2####



# Table 4 ####
# 複利計算の死亡率　mortality　はどうやって計算した?　20260402　####

Population_growth("Bunazaka")
Population_growth("Kaminokodaira")
Population_growth("Matsuotoge")
Population_growth("Kagamiishi")


#' #11   オオシラビソ  30  17  15 24 -2.3666002 2.8881133  0.5215131
DemographicRates(n0=30, n1=17, nS=15,t=24)


#Tateyama_reviewer2_statistics.R


#Annual mortality rate was calculated as 1 − (1 − p)(1/d)

#' discrete Mortality rate
#'  1 - (1 - p)^(1/d)
#'
#' @param p probability
#' @param d duration
#'
#' @returns MortalityRate
#'
#' @export
#'
#' @examples
#' Abies_death_ratio
#' MortalityRate(0.50000000,24)
#' #0.02846806
#'
MortalityRate<-function(p,d){
  1 - (1 - p)^(1/d)
}




# Kaminokodaira
# 全期間 annual survival = 0.9715 / mortality = 0.0285
(1-0.0285)^24
# Matsuotoge
# 全期間 annual survival = 0.9915 / mortality = 0.0085
# Kagamiishi
# 全期間 annual survival = 0.9931 / mortality = 0.0069

# 最初から生きていた木
# 最初から最後まで生きていた木
#　
#　最後に生きていた木=最初から最後まで生きていた木+途中から加入した木+途中で死んだ木
#　途中から入って途中で死んだ木---conditでは計算に含めない
#　連続複利計算式
#

