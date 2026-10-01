# alt_correct.R
load("RData_obj.RData")
RData_obj

# s <- new_statistics ok!! ####

s <- new_statistics(
  wi_year          = wi_year,
  Abies_death_ratio = Abies_death_ratio,
  f1_raw           = data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
  f2_raw           = data_Fig_yr_ba_kaminokodaira_zone_2024
)

# TemperatureWIAbiesPopulation ####

d<-TemperatureWIAbiesPopulation
d <- data.frame(d,WI1368=d$WI)
for(i in 1:nrow(d)){
  wi.<-mean(wi_year[match(d$yr1[i]:d$yr2[i],wi_year$year),d$plot[i]])
  d$WI[i]<-wi.
}
d

TemperatureWIAbiesPopulation <- d

# save(TemperatureWIAbiesPopulation, file="data/TTemperatureWIAbiesPopulation.RData")   # alt_correct.R 20261001

# TemperatureWIAbies_Population_BA_Mortality.RData ####


d<-TemperatureWIAbies_Population_BA_Mortality2
d <- data.frame(d,WI1368=d$WI)
for(i in 1:nrow(d)){
  wi.<-mean(wi_year[match(d$yr1[i]:d$yr2[i],wi_year$year),d$plot[i]])
  d$WI[i]<-wi.
}
d
TemperatureWIAbies_Population_BA_Mortality2 <- d
# save(TemperatureWIAbies_Population_BA_Mortality2, file="data/TemperatureWIAbies_Population_BA_Mortality.RData")   # alt_correct.R 20261001


# Fig.11 ####
## Abies ####
d <- data.frame(Abies,WI1368=Abies$WI)
for(i in 1:nrow(d)){
  wi.<-wi_year_lm[which(wi_year_lm$year==d$year[i]),d$plot[i]]
  d$WI[i]<-wi.
}
d

Abies<-d
# save(Abies, file="data/Abies.RData")


# wi_year_lm #### smoothed WI with regression line
wi_year
wi_year_lm <- wi_year
for ( j in c("Bunazaka", "Kaminokodaira", "Matsuotoge", "Kagamiishi")){
  res <- lm(wi_year[,j]~wi_year$year)
  wi_year_lm[,j] <- predict(res)
}

wi_year_lm

# save(wi_year_lm, file="data/wi_year_lm.RData")

# Fig.7 Fig_wi_ba_cor_ancova_JVS2() ####
# sitr : Kaminokodaira
Fig_wi_ba_cor_ancova_JVS2()
dz    <- data_Fig_yr_ba_kaminokodaira_zone_2024
dsp <- data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024

dz2    <- data.frame(dz,WI1368=dz$WI)
dsp2<-data.frame(dsp,WI1368=dsp$WI)


# use lm prediction value
x<-wi_year[,"year"]
y<-wi_year[,"Kaminokodaira"]
res<-lm(y~x)
  plot(x,y)
abline(res)
dz2$WI <- predict(res,newdata=data.frame(x=dz$year))
dsp2$WI <- predict(res,newdata=data.frame(x=dsp$year))



data_Fig_yr_ba_kaminokodaira_zone_2024 <- dz2
data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024 <- dsp2

# save(data_Fig_yr_ba_kaminokodaira_zone_2024,file="data/data_Fig_yr_ba_kaminokodaira_zone_2024.RData")

# save(data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,file="data/data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024.RaDta")



# data_Fig_yr_ba_kaminokodaira_zone_2024 ####
# data_Fig_yr_ba_kaminokodaira_zone_2024.RData
d <- data_Fig_yr_ba_kaminokodaira_zone_2024
d <- data.frame(d,WI1368=d$WI)
for(i in 1:nrow(d)){ #i=1
  wi.<-wi_year[which(wi_year$year==d$year[i]),"Kaminokodaira"]
  d$WI[i]<-wi.
}
d
plot(d$WI,d$WI1368m)
plot(d$WI,type="l")
#yr_wi_bar <- d

data.frame(names(plt5))
tmp <- as.numeric(plt5[1,seq(77,110,3)]/10)
plot(tmp)
wi_calc(tmp)
wi_calc(tmp+0.5)

# yr_wi_bar ####
wi_year
yr5
yr_wi_bar

yr_wi_bar_1368m <- read.csv("yr_wi_bar_1368m.csv") #yr_wi_bar
# save(yr_wi_bar_1368m,file="yr_wi_bar_1368m.RData")
# write.csv(yr_wi_bar_1368m,file="yr_wi_bar_1368m.csv")


d <- data.frame(yr_wi_bar,wi1368m=yr_wi_bar$wi)

for(i in 1:nrow(d)){
  wi.<-wi_year[which(wi_year$year==d$yr[i]),d$na[i]]
  d$wi[i]<-wi.
}
d
plot(d$wi,d$wi1368m)
plot(d$wi,type="l")
yr_wi_bar <- d

# save(yr_wi_bar, file="../../data/yr_wi_bar.RData")  #20260930 14:28

# RData_obj ####
source("rdata_objects.R")

RData_obj <- rdata_objects("../../data/")
#save(RData_obj,file="RData_obj.RData")
load("data_raw/alt_correct/RData_obj.RData")
RData_obj



# wi RData ####
f<-dir("../../data",pattern = "wi", ignore.case = TRUE)


fl <- data.frame(
  file   = f,
  source = NA,   # 生成元のスクリプト
  paper  = NA,   # 論文で使用している図表(例: Fig.3, Table 2)
  status = "todo",
  note   = NA
)

# copy old wi RData to old/ ####
# dir.create("old")
# file.copy(paste0("../../data/",f), "old/", copy.date = TRUE)

#

# YearsIntervalCalc_plots_temp.csv ####

# write.csv(fl, "alt_correct_list.csv", row.names = FALSE)

#' df <- kurobe_dam_temperature
#' str(df)
#' df1<-YearsIntervalCalc_plots(plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi"),df=df,data.clm="min",year.clm="year",method="min")
#'df2<-YearsIntervalCalc_plots(plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi"),df=df,data.clm="max",year.clm="year",method="max")
#'df3<-YearsIntervalCalc_plots(plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi"),df=df,data.clm="mean",year.clm="year",method="mean")
#' d<-data.frame(df1,Tmax=df2$max,Tmean=df3$mean)
#'  names(d)[names(d)=="min"]<-"Tmin"
#'df.temp<-d
#' #標高補正　(dt<--0.55/100*(plt4$alt-1368))
#'  plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi")
#'  for(ii in 1:3){
#'  plot.<-plot.name[ii]
#'  #dt.<- -0.55/100*(plt5$alt[plt5$na== plot.]-1368)
#'  dt.<- -0.55/100*(plt5$alt[plt5$na== plot.]-1459)
#'  df.temp$Tmin[df.temp$plot==plot.] <-df.temp$Tmin[df.temp$plot==plot.]+dt.
#'   df.temp$Tmax[df.temp$plot==plot.] <-df.temp$Tmax[df.temp$plot==plot.]+dt.
#'    df.temp$Tmaen[df.temp$plot==plot.] <-df.temp$Tmean[df.temp$plot==plot.]+dt.
#'  }
#'
#'  # write.csv(df.temp,file="data_raw/YearsIntervalCalc_plots_temp.csv")
#'  # write.csv(df.temp,file="YearsIntervalCalc_plots_temp.csv")

# RData ####
setwd("~/Dropbox/00D/00/tateyama/TateyamaForest/TateyamaForest/data_raw/alt_correct")
fl<-read.csv("rdata_objects_old.csv")
(fl2<-subset(fl,file!="wi_yaer.RData"))
## wi_yaerは必要? ####
head(wi_yaer)
head(wi_year)
(fl[!fl$file=="",])
# 同じものなので　data/wi_yaer.RData　は削除
file.info(dir("old/", full.names = TRUE))
file.info(dir())
## old vs new ####
head(WI_with_KurodeDamObservation)
head(WI_with_KurobeDamObservation)
head(WI_with_KurodeDamObservation_1965_2024_AllForestPlots)
head(WI_with_KurobeDamObservation_1965_2024_AllForestPlots)

