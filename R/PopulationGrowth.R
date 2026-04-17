# PopulationGrowth_Condit.R



#' Population growth (Condit  et al.)
#' save(PopulationGrowth,file="data/PopulationGrowth.RData")
#'
#'
#' @param plot.   default = "Kaminokodaira".
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#'  Population_growth("Kaminokodaira")
#'  str(Population_growth("Kaminokodaira"))
#'   Population_growth("Kagamiishi")
#'   Population_growth("Bunazaka")
#' l<-lapply(plt2$na,Population_growth)
#' names(l)<-plt2$na
#' (PopulationGrowth <- l)
#'
#' # save(PopulationGrowth ,file="data/PopulationGrowth.RData")
#'
#' #' # Table作成 ####
#' spl<-SpeciesList5
#' l<-PopulationGrowth
#' l..<-c()
#' for(ii in 1:length(l)){
#'   l.<-l[[ii]]
#'   zone <- spl$zone[match(l.$sp,spl$spj)]
#'   l..<-rbind(l..,data.frame(plot=names(l)[ii],zone,l.))
#' }
#'
#' (PopulationGrowth_df <-l..)
#' # save(PopulationGrowth_df ,file="data/PopulationGrowth_df.RData")
#' # write.csv(PopulationGrowth_df,file="data_raw/PopulationGrowth_df.csv")
#'
#'
#' l<-PopulationGrowth_df
#' plot.zone <- data.frame(
#'   plot= c("Bunazaka","Kaminokodaira","Kaminokodaira","Kaminokodaira","Matsuotoge","Kagamiishi"),
#'   zone= c("Temperate","Temperate","Ecotone","Subarctic","Subarctic","Subarctic")
#' )
#'
#' plot.zone
#'
#' l..<-c()
#' for(ii in 1:nrow(plot.zone)){
#'   l.<-subset(l,plot== plot.zone[ii,1] & zone==plot.zone[ii,2])
#'   nt<-c(sum(l.$n0),sum(l.$n1),sum(l.$nS),l.$t[1])
#'   l..<-rbind(l..,DemographicRates(nt[1],nt[2],nt[3],nt[4]))
#' }
#'
#'
#' (PopulationGrowth_zone_df<-data.frame(plot.zone,l..))
#'
#' # save(PopulationGrowth_zone_df ,file="data/PopulationGrowth_zone_df.RData")
#'
#' # write.csv(PopulationGrowth_zone_df ,file="data_raw/PopulationGrowth_zone_df.csv")
#'
#' # 各プロット全種DemographicRates ####
#' l<-PopulationGrowth_zone_df
#' l$sp
#' l<-data.frame(cbind(tapply(l$n0,l$plot,sum),tapply(l$n1,l$plot,sum),tapply(l$nS,l$plot,sum)  ) ,c(22,24,24,23) )
#' names(l)<-c("n0","n1","nS","t")
#' unique(l$plot)
#'
#' DemographicRates_all <- DemographicRates(l$n0,l$n1,l$nS,l$t)
#' rownames(DemographicRates_all)<-rownames(l)
#' DemographicRates_all
#'
Population_growth <- function(plot.="Kaminokodaira"){
  ii<-match(plot.,plt2$na)
  t<- plt2$yr7[ii]-plt2$yr1[ii]   # 調査期間　(年)

  l. <- subset(dd5,plot==plot.)
  sp.<- unique(l.$sp)

  pg <- data.frame(
    sp = as.character(NA),  # 文字列 (string)
    n0 = as.integer(NA),    # 整数 (integer)
    n1 = as.integer(NA),
    nS = as.integer(NA),
    t  = as.integer(NA),
    p  = as.numeric(NA),    # 実数 (real)
    m  = as.numeric(NA),
    r  = as.numeric(NA)
  )
  pg<-pg[-1,]
  for(i in 1:length(sp.)){
    sp.. <- sp.[i]
    l<-subset(dd5,plot==plot. & sp==sp..)

    # plot.="Kagamiishi"

    if(plot.!="Kagamiishi"){
      n0 <- sum(l$d01>=10 & l$f01>0,na.rm=T)
      n1 <- sum(l$d07>=10 & l$f07>0,na.rm=T)
      nS <- sum(l$f01>0 & l$f07>0,na.rm=T)
    } else {
      n0 <- sum(l$f01>0,na.rm=T)
      n1 <- sum(l$f07>0,na.rm=T)
      nS <- sum(l$f01>0 & l$f07>0,na.rm=T)
    }

  # c(n0,n1,nS)

    p <- log(n1/n0)/t *100
    m <- log(n0/nS)/t *100
    r <- log(n1/nS)/t *100
    #pg[i,]<-c(sp..,n0,n1,nS,p,m,r)
    pg <- rbind(pg, data.frame(sp = sp.., n0 = n0, n1 = n1, nS = nS, t=t, p = p, m = m, r = r))
  }


  return(pg)
}


#' DemographicRates  (Population growth : Condit  et al.)
#'   p <- log(n1/n0)/t *100
#'  m <- log(n0/nS)/t *100  Instantaneous Mortality Rate
#'  r <- log(n1/nS)/t *100
#'
#' @param n0   Population size at first survey
#' @param n1   Population size at latest survey
#' @param nS   Population size of survivors
#' @param t    census intervals (years)
#'
#' @return
#' @export
#'
#' @examples
#' DemographicRates(n0=100, n1=80, nS=50,t=24)
#'  # n0 n1 nS  t          p        m        r
#'  # 1 100 80 50 24 -0.9297648 2.888113 1.958348
#'  # m % instantaneous mortality rate
#'  100*exp(-0.02888113*24)
#'
#' DemographicRates(n0=100, n1=150, nS=80,t=10)
#' DemographicRates(n0=100, n1=100, nS=100,t=1)
#' DemographicRates(n0=100, n1=110, nS=100,t=1)
#'
DemographicRates <- function(n0=100, n1=150, nS=80,t=10){
  p <- log(n1/n0)/t *100
  m <- log(n0/nS)/t *100
  r <- log(n1/nS)/t *100
  return(data.frame(n0, n1, nS,t,p,m,r))
}


#' discrete Mortality rate
#'  "discrete"   1 - (1 - p)^(1/d)
#'  "instantaneous"  log(1/p)/d
#'
#' @param p proportion of trees that DIED during the interval
#' @param d duration
#' @param method "instantaneous"(default),"discrete"
#'
#' @returns Mortality Rate
#'
#' @export
#'
#' @examples
#'  MortalityRate(0.48000000,24,method="instantaneous")
#'  # 0.03058205
#'  exp(-0.03058205*24)
#'
#' Abies_death_ratio
#' # Kaminokodaira discrete
#' MortalityRate(0.50000000,24,method="discrete")
#' # 0.02687909
#' 1-(1-0.02687909)^24
#'
#' # Kaminokodaira "instantaneous"
#'
#' MortalityRate(0.50000000,24)  #2000-2024
#' exp(-0.02888113*24)
#'
#' MortalityRate(0.40000000,11)  #2013-2024 (0.50000000-0.10000000)
#' # 0.08329916
#' exp(- 0.08329916*11)
#'
MortalityRate<-function(p,d,method=c("instantaneous","discrete")){
  method <- match.arg(method)

  result <- switch(method,
                   "discrete" = {
                     1 - (1 - p)^(1/d)
                   },
                   "instantaneous" = {
                     log(1/p)/d
                   }
  )

  return(result)

}


#' YearsIntervalAverage
#'
#' @param yr
#' @param v
#' @param yr.interval
#'
#' @returns
#'
#' @export
#'
#' @examples
#' plot_name<-"Kagamiishi"
#' yr.interval<-as.numeric(subset(plt5,na==plot_name)[,paste0("yr",1:7)])
#' yr<-as.numeric(rownames(wi_year))
#' v<-as.numeric(wi_year[,plot_name])
#' YearsIntervalAverage(yr,v,yr.interval)
#'YearsIntervalCalc(yr,v,yr.interval,"mean")
YearsIntervalAverage<-function(yr,v,yr.interval){
  m<-c()
  n.yr<-match(yr.interval,yr)
  for(i in 1:(length(n.yr)-1)){
    m<-c(m,mean(v[n.yr[i]:n.yr[i+1]],na.rm=TRUE))
  }
  return(m)
}





# climate conditins ####


## wi ####
#' YearsIntervalAverage_plots
#'
#' @param plot.name
#' @param yr
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
#' plot.name <- c( "Kaminokodaira","Matsuotoge","Kagamiishi")
#' df<-data.frame(wi_year); df$year=as.numeric(rownames(wi_year))
#' d<-YearsIntervalAverage_plots_wi(plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi"),df=df,year.clm="year")
#' YearsIntervalAverage_plots_wi_data<-d
#' # save(d,file="data_raw/YearsIntervalAverage_plots_wi_data.RData")
YearsIntervalAverage_plots_wi <- function(plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi"),df=df,year.clm="year"){
  d<-c()
  for(ii in 1:length(plot.name)){
    plot_name<-plot.name[ii]
    yr.interval<-as.numeric(subset(plt5,na==plot_name)[,paste0("yr",1:7)])
    yr<-df[ ,year.clm]
    v<-df[, plot_name]
    d<-cbind(d,YearsIntervalAverage(yr,v,yr.interval))

  }
  d<-data.frame(d)
  names(d)<-plot.name
  return(d)
}

#' YearsIntervalCalc
#'
#' @param yr
#' @param v
#' @param yr.interval
#' @param method
#'
#' @returns
#' @export
#'
#' @examples
#'
#' # Dam_Tmin_max ####
#' d <- kurobe_dam_temperature
#' plot_name<-"Kagamiishi"
#' yr.interval<-as.numeric(subset(plt5,na==plot_name)[,paste0("yr",1:7)])
#' yr<-d$year
#' v<-d$max
#' YearsIntervalCalc(yr,v,yr.interval)
#' YearsIntervalCalc(yr,v,yr.interval,"mean")
#' YearsIntervalCalc(yr,v,yr.interval,"max")
#' YearsIntervalCalc(yr,v,yr.interval,"min")
#'
#'
YearsIntervalCalc<-function(yr,v,yr.interval,method=c("mean","min","max")){
  method <- match.arg(method)
  result<-c()
  n.yr<-match(yr.interval,yr)


  for(i in 1:(length(n.yr)-1)){
    i12<-n.yr[i]:n.yr[i+1]
    #result<-c(result,mean(v[i12],na.rm=TRUE))

    result <- switch(method,
                     "mean" = {
                       c(result,mean(v[i12],na.rm=TRUE))
                     },
                     "min" = {
                      c(result,min(v[i12],na.rm=TRUE))
                     },
                     "max" = {
                       c(result,max(v[i12],na.rm=TRUE))
                     }
                     )
  }
  return(result)
}




#' Title
#'
#'
#' @param plot.name
#' @param df
#' @param year.clm
#' @param method   "mean","min","max"
#'
#' @returns
#' @export
#'
#' @examples
#'
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
#'  dt.<- -0.55/100*(plt5$alt[plt5$na== plot.]-1368)
#'  df.temp$Tmin[df.temp$plot==plot.] <-df.temp$Tmin[df.temp$plot==plot.]+dt.
#'   df.temp$Tmax[df.temp$plot==plot.] <-df.temp$Tmax[df.temp$plot==plot.]+dt.
#'    df.temp$Tmaen[df.temp$plot==plot.] <-df.temp$Tmean[df.temp$plot==plot.]+dt.
#'  }
#'
#'  # write.csv(df.temp,file="data_raw/YearsIntervalCalc_plots_temp.csv")
#'
#'
#'  # Warmth Index
#'  d<-YearsIntervalAverage_plots_wi_data
#'  d<-c(d$Kaminokodaira,d$Matsuotoge,d$Kagamiishi)
#'  TempWi<-data.frame(df.temp,wi=d)
#'   # write.csv(TempWi,file="data_raw/df_temp_wi.csv")
#'
YearsIntervalCalc_plots <- function(plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi"),df=df,
                                    data.clm="min",year.clm="year",method="min"){
  #method <- match.arg(method)

  d<-c()
  for(ii in 1:length(plot.name)){
    plot_name<-plot.name[ii]
    yr.interval<-as.numeric(subset(plt5,na==plot_name)[,paste0("yr",1:7)])
    yr<-df[ ,year.clm]
    v<-df[, data.clm]
    period=1:(length(yr.interval)-1)
    yr1=yr.interval[-length(yr.interval)]
    yr2=yr.interval[-1]
    d.<-data.frame(plot=plot_name, period,yr1,yr2,method=YearsIntervalCalc(yr,v,yr.interval,method))
   # d<-cbind(d,YearsIntervalCalc(yr,v,yr.interval,method))
    d<-rbind(d,d.)
  }
  #d<-data.frame(d)
  #names(d)<-plot.name
  names(d)[names(d)=="method"]<-method
  return(d)
}

# Abies ######################
# plot.name=c( "Kaminokodaira","Matsuotoge","Kagamiishi")
# tw<-read.csv("df_temp_wi.csv")
#
# dr<-c()
# for (ii in 1:length(plot.name)){
#   plot.=plot.name[ii]
#   d<-subset(dd5,plot==plot. & sp=="オオシラビソ")[,paste0("f",substr(101:107,2,3))]
#   for(j in 1:6){
#     t<-subset(tw,plot==plot.)$yr2[j]-subset(tw,plot==plot.)$yr1[j]
#     n0<-sum(d[,j]>1,na.rm=T)
#     nS<-sum(d[,j]>1 & d[,j+1]>1,na.rm=T)
#     n1<-sum(d[,j+1]>1,na.rm=T)
#     dr<-rbind(dr,DemographicRates(n0, n1, nS,t))
#   }
#
# }
#
# TemperatureWIAbiesPopulation<-data.frame(tw,dr)
# # write.csv(TemperatureWIAbiesPopulation,file="TemperatureWIAbiesPopulation.csv")
# # save(TemperatureWIAbiesPopulation,file="TemperatureWIAbiesPopulation.RData")


# 2026/04/14
# 黒部ダム観測値の変更に伴う訂正　####
# d<-YearsIntervalAverage_plots_wi_data
#
# z<-TemperatureWIAbiesPopulation
# z$WI<-c(d[,1],d[,2],d[,3])
# d2<-read.csv("data_raw/YearsIntervalCalc_plots_temp.csv")
# d2
# z$Tmin<-d2$Tmin
# z$Tmax<-d2$Tmax
# z$Tmean<-d2$Tmean
# z<-z[,-c(1,2)]
# TemperatureWIAbiesPopulation<-z
# # save(TemperatureWIAbiesPopulation,file="TemperatureWIAbiesPopulation.RData")




# 以下は利用なし　>>>>>####
# # correlation between Toyama Meteorological station and Kurobe
# kuro<-kurobe_dam_temperature
# toya<-ToyamaMet_1939_2025
# i <- match(kuro$year,toya$year)
# toya_Tmin<-toya$Tmin[i]
# toya_Tmax<-toya$Tmax[i]
# toya_Tmean<-toya$Tmean[i]
#
# toya[match(c(2003,2004,2014),toya$year),]
# toya$year
#
# kurobe_toyama<-data.frame(kuro,toya_Tmin,toya_Tmax,toya_Tmean)
# # save(kurobe_toyama,file="kurobe_toyama.RData")
# # write.csv(kurobe_toyama,file="../data_raw/kurobe_toyama.csv")
#
# cor(kurobe_toyama$max,kurobe_toyama$toya_Tmax)
#
#
# z<-WI_with_KurodeDamObservation
# tm<-toya[27:85,"Tmean"]
# plot(tm,z[,1])
# cor.test(tm,z[,1])
# cor.test(tm,z[,4])
