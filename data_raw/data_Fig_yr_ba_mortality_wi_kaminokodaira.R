# data_Fig_yr_ba_wi_kaminokodaira.R



.<-WI_with_KurodeDamObservation_1965_2024_AllForestPlots
ans<-lm(.$Kaminokodaira~.$year)
summary(ans)
plot(.$year,.$Kaminokodaira)
abline(ans)


setwd("C:/Users/zero/Dropbox/00D/00/tateyama/TateyamaForest/TateyamaForest/data_raw")
dir()

load("YearsIntervalAverage_plots_wi_data.RData")
d
.<-TemperatureWIAbies_Population_BA_Mortality
..<-data.frame(.,WI_old=.$WI)

..$WI<-unlist(d)
..
TemperatureWIAbies_Population_BA_Mortality2<-..
TemperatureWIAbies_Population_BA_Mortality<-TemperatureWIAbies_Population_BA_Mortality2
#save(TemperatureWIAbies_Population_BA_Mortality2,file="../data/TemperatureWIAbies_Population_BA_Mortality.RData")
#file.copy("../data/TemperatureWIAbies_Population_BA_Mortality.RData","../data/TemperatureWIAbies_Population_BA_Mortality_old.RData")
.data_Fig_yr_ba_kaminokodaira_zone_2024
.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024
TemperatureWIAbies_Population_BA_Mortality
#.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024' not found


TemperatureWIAbies_Population_BA_Mortality
wi_year

plot.<-c("Kaminokodaira", "Matsuotoge", "Kagamiishi")
ii<-1
x<-wi_year$year
y<-wi_year[,plot.[ii]]
plot(x,y)
res<-lm(y~x)
abline(res)
predict(res)

# data_Fig_yr_ba_kaminokodaira_zone_2024 #####
z<-.data_Fig_yr_ba_kaminokodaira_zone_2024
WI_reg<-predict(res)[match(z$year,x)]
z2<-data.frame(z, WI_reg)
plot(z2$WI_reg[8:14],z2$ba_ratio[8:14])
# data_Fig_yr_ba_kaminokodaira_zone_2024<-z2
#save(data_Fig_yr_ba_kaminokodaira_zone_2024,file="../data/data_Fig_yr_ba_kaminokodaira_zone_2024.RData")
data_Fig_yr_ba_kaminokodaira_zone_2024$WI<-WI_reg
data_Fig_yr_ba_kaminokodaira_zone_2024
#save(data_Fig_yr_ba_kaminokodaira_zone_2024,file="../data/data_Fig_yr_ba_kaminokodaira_zone_2024.RData")


# .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024 ####
z<-.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024
z2<-data.frame(z,WI_reg)
plot(z2$WI_reg[8:14],z2$ba_ratio[8:14])
# data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024<-z2
#save(data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,file="../data/data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024.RData")
names(data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024)<-c("sp","year", "ba_ratio","WI_mean", "WI")
data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024
#save(data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,file="../data/data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024.RData")

# Fig_Abies_wi_ba_mortality これは使わない
z<-Abies_death_ratio
plot.<-c("Kaminokodaira", "Matsuotoge", "Kagamiishi")

d<-c()
z2<-z
for(ii in plot.){
  x<-wi_year$year
  y<-wi_year[,ii]
  WI_reg<-predict(lm(y~x))[match(z[[ii]]$year,x)]
  z2[[ii]]<-data.frame(z2[[ii]],WI_reg)
  d<-rbind(d, data.frame(plot=ii,z[[ii]],WI_reg))
}
Abies_death_ratio<-z2
#save(Abies_death_ratio,file="../data/Abies_death_ratio_wi.RData")
data_Abies_death_ratio_wi<-d
#save(data_Abies_death_ratio_wi,file="../data/data_Abies_death_ratio_wi.RData")

# AbiesPopulation_wi_BAratio_mortarity
data_Abies_death_ratio_wi

data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024

#plot.<-c("Kagamiishi", "Matsuotoge", "Kaminokodaira")

plot.<-c( "Kaminokodaira", "Matsuotoge","Kagamiishi")
.<-sp_ba
d<-c()
for(ii in plot.){　# ii="Kaminokodaira"
  ..<-.[[ii]]["オオシラビソ",]
year.<-as.numeric(names(..))
BAratio.<-as.numeric(..)
BAratio.<-BAratio./BAratio.[1]
# WI regression
x<-wi_year$year
y<-wi_year[,ii]
res<-lm(y~x)
WI.<-predict(res)[match(year.,x)]
  d.<-data.frame(plot=ii,year=year.,BAratio=BAratio.,mortality=0,WI=WI.)
  d<-rbind(d,d.)
}
d$mortality<- data_Abies_death_ratio_wi$death_ratio
Abies<-d
# save(Abies,file="data/Abies.RData")




# WI correct ####
.data_Fig_yr_ba_kaminokodaira_zone_2024$WI
data_Abies_death_ratio_wi$WI_reg
.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024
wi.<-subset(Abies,plot=="Kaminokodaira")$WI

.data_Fig_yr_ba_kaminokodaira_zone_2024$WI<-rep(wi.,3)

#save(.data_Fig_yr_ba_kaminokodaira_zone_2024,file="../data/.data_Fig_yr_ba_kaminokodaira_zone_2024.RData")

.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024<-data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024

#save(.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,file="../data/.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024.RData")
