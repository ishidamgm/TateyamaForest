### kurobe_dam_temperature.R

# data_frame ####
d<-read.csv("data_raw/黒部ダム　年間平均気温（1965年～2023年）.csv",skip=1)
names(d)<-c("wareki","year","min","max")
# d[39,3]<-(3.5+4.4)/2 ;d[39,4]<-(11.3+11.6)/2
# d[41,3]<-(4.4+2.3)/2 ;d[41,4]<-(11.6+11.3)/2
# d[50,3]<-(2.1+2.1)/2 ;d[50,4]<-(11.0+11.8)/2
i<-which(is.na(d$max))
d$year[i]  #2003 2005 2014 欠測
d<-d[-i,]
d$min<-as.numeric(d$min)
d$mean<-(d$max+d$min)/2
d
# plot ####
plot(d$year,d$max,type="b",col="red",ylim=c(0,14),xlab="Year",ylab="Temperature (degree Celsius)")
lines(d$year,d$min,type="b",col="blue")
lines(d$year,d$mean,type="b")

# regression ####
lm_max<-lm(d$max~d$year)
lm_min<-lm(d$min~d$year)
lm_mean<-lm(d$mean~d$year)
lm_<-list(summary(lm_max),summary(lm_min),summary(lm_mean))


# regression lines ####
abline(lm(d$max~d$year),col="red")
abline(lm(d$min~d$year),col="blue")
abline(lm(d$mean~d$year))

# regression analysis ####
lm_[[3]]
kurobe_dam_temperature_regression <-
  data.frame(Annual_mean_temperature=c("daily_Maximum","daily_Mean","daily_Minimum"),
           Intercept=c(-91.455023,-47.93241,-4.409797),
           Pr.1=c("***","***",""),
           Coefficient=c(0.051130, 0.02734,0.003551),
           Pr.2=c("***","***",""),
           Adjusted_R_squared=c(0.7137,0.405,-0.01128),
           p_value=c("< 2.2e-16","8.142e-08","0.5368"),
           Pr.3=c("***","***","")
)

kurobe_dam_temperature_regression

# save ####
kurobe_dam_temperature<-d
# save(kurobe_dam_temperature,file="data/kurobe_dam_temperature.RData")
# save(kurobe_dam_temperature_regression,file="data/kurobe_dam_temperature_regression.RData")

# 暫定推定 kurobe_dam_temperature_2024 ####
# 富山気象台のデータを用いた暫定推定  変化率同じの前提

library(clipr)
ToyamaMet <- read_clip_tbl()
ToyamaMet_1939_2025 <- ToyamaMet[,c("year", "Tmean", "Tmax","Tmin" )]
# saveRDS(ToyamaMet_1939_2025,file="ToyamaMet_1939_2025.rds")
toya<-ToyamaMet_1939_2025


ToyamaKishoudaiTemp<-data.frame(yaer=2022:2024,max=c(15.1,16.1,15.9),min=c(11.2,11.9,12.2),mean=c(11.2,11.9,12.2))
ToyamaKishoudaiTemp

TmaxEstimate_from2023to2024<-function(tmax2023)15.9/16.1*tmax2023
TminEstimate_from2023to2024<-function(tmin2023)12.2/11.9*tmin2023
#TmeanEstimate_from2023to2024<-function(tmean2023)12.2/11.9*tmean2023


min=TminEstimate_from2023to2024(2.8)
max=TmaxEstimate_from2023to2024(12.7)
mean=(max+min)/2
(est2024 <-data.frame(wareki="R6", year=2024, min , max, mean))

kurobe_dam_temperature_2024estimated<-rbind(kurobe_dam_temperature,est2024)
tail(kurobe_dam_temperature_2024estimated)
# saveRDS(kurobe_dam_temperature_2024estimated,file="kurobe_dam_temperature_2024estimated.rds")

#欠測年#
#library(tidyverse)
kuro<-kurobe_dam_temperature_2024estimated
#as_tibble(kuro)
yr_lack<-kuro$year[which(diff(kuro$year)==2)]+1　　#　2003 2005 2014
(kuro.<-kuro[match(yr_lack-1,kuro$year),])
kuro..<-kuro.
kuro..$year<-yr_lack
kuro..$wareki<-""
#

for(ii in 1:nrow(kuro..)){
  i<-which(toya$year==yr_lack[ii])

  rate.min<-toya[i,"Tmin"]/toya[i-1,"Tmin"]
  rate.max<-toya[i,"Tmax"]/toya[i-1,"Tmax"]
  rate.mean<-toya[i,"Tmean"]/toya[i-1,"Tmean"]
  kuro..[ii,"min"]<-round(rate.min*kuro..[ii,"min"],2)
  kuro..[ii,"max"]<-round(rate.max*kuro..[ii,"max"],2)
  kuro..[ii,"mean"]<-round(rate.mean*kuro..[ii,"mean"],2)
}
kuro...<-rbind(kuro,kuro..)
kuro...<-kuro...[order(kuro...$year),]
nrow(kuro)
nrow(kuro...)

# saveRDS(kuro...,file="kurobe_dam_temperature_2024estimated.rds")
