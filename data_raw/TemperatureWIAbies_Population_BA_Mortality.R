# TemperatureWIAbies_Population_BA_Mortality.R
TemperatureWIAbiesPopulation
Abies_death_ratio
Fig_Abies_death_ratio()
Fig_yr_ba_site2()

Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024()
names(sp_ba_ratio)



.<-TemperatureWIAbies_Population_BA_Mortality
plot.<-c("Kaminokodaira","Matsuotoge","Kagamiishi")

par(mfrow=c(2,1))
## WI-Mortality ####
plot(0,xlab="WI",ylab="Basal area ratio",xlim=c(22,57),ylim=c(0.78,1.2))
for (ii in 1:length(plot.)){
 d<-subset(.,plot==plot.[ii])
 points(d$WI,d$BAratio,type="b",col=ii)
}
## WI-Mortality ####
plot(0,xlab="WI",ylab="Mortality",xlim=c(22,57),ylim=c(0,0.4))
for (ii in 1:length(plot.)){
  d<-subset(.,plot==plot.[ii])
  points(d$WI,d$mortality,type="b",col=ii,pch=2)
}




#' Abies_mortality_ratio_midpoint
#'
#' @returns
#' @export
#'
#' @examples
#' mortality<-unlist(Abies_mortality_ratio_midpoint())
#' BAratio<-unlist(Abies_ba_ratio_midpoint())
#' TemperatureWIAbies_Population_BA_Mortality<-data.frame(TemperatureWIAbiesPopulation,BAratio,mortality)
#' # save(TemperatureWIAbies_Population_BA_Mortality,file="data/TemperatureWIAbies_Population_BA_Mortality.RData")
Abies_mortality_ratio_midpoint<-function(){
  .<-Abies_death_ratio

  plot.<-c("Kaminokodaira","Matsuotoge","Kagamiishi")
  d<-c()
  for (ii in 1:length(plot.)){
    d.<-.[[plot.[ii]]]$death_ratio
    d<-c(d,list((d.[-1]+d.[-length(d.)])/2))
  }
  names(d)<-plot.
  return(d)
}


#' Abies_ba_ratio_midpoint
#'
#' @returns
#' @export
#'
#' @examples
#' Abies_ba_ratio_midpoint()
Abies_ba_ratio_midpoint<-function(){
  .<-sp_ba
  sp.<- "オオシラビソ"
  plot.<-c("Kaminokodaira","Matsuotoge","Kagamiishi")
  rba.<-c()
  for (ii in 1:length(plot.)){
    bar.<-.[[plot.[ii]]]
    rba.sp <-bar.[sp.,]/bar.[sp.,1]
    rba.<-c(rba.,list((rba.sp[-1]+rba.sp[-length(rba.sp)])/2))
  }
  names(rba.)<-plot.
  return(rba.)
}

