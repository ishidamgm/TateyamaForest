

# 積算死亡率　径級別 ####
#　死亡率　どのくらいの確率で死亡するか

#plt.<-c("Kaminokodaira","Matsuotoge","Mimatsu","Kagamiishi")
plr<-4:7
d.<-subset(dd3,plot=="Kaminokodaira" & sp=="ooshirabiso" & f01>0)
nrow(d.)
(f.<-d.[,c(clm_f)])
dbh.<-d.$d06
cls<-seq(10,110,10)
dbh.cls<-cut(dbh.,cls.)
dbh.cls.dead<-dbh.cls[d.$f06<1]
cumsum(table(dbh.cls.dead))/nrow(d.)


#' Title
#'
#' @return
#' @export
#'
#' @examples
#' Fig_cunsumDeathRatio()
#'
Fig_cunsumDeathRatio<-function(){
  cls.<-seq(0,110,5)

  cunsumDeathRatio<-c()
  for(ii in plr){  #ii<-1
    d.<-subset(dd3,pn==ii  & sp=="ooshirabiso" & f01>0 & !is.na(d01))
    (f.<-d.[,c(clm_f)])
    dbh.<-d.$d01
    dbh.cls<-cut(dbh.,cls.)
    dbh.cls.dead<-dbh.cls[d.$f06<1]
    cunsumDeathRatio.<-cumsum(table(dbh.cls.dead))/nrow(d.)
    cunsumDeathRatio<-c(cunsumDeathRatio,list(cunsumDeathRatio.))
  }
  par(mfrow=c(1,1))

  plot(0,type="n" ,
       xlim=c(0,110) , ylim=c(0,0.25),
       xlab="DBH (cm)",ylab="Cumulative Death Ratio",main="A. mariesii")

  for (ii in 1:length(cunsumDeathRatio)){
    jj<-plr[ii]
    lines(cls.[-1],cunsumDeathRatio[[ii]],type="b",
                       lty=leg$lty[jj],pch=leg$pch[jj],col=leg$col[jj])
  }

  legend(1,0.25,leg$n[plr],pch=leg$pch[plr],col=leg$col[plr],lty=leg$lty[plr],cex=0.7)

}

cls.


#' table(as.numeric(as.matrix(f.)))
#'
#'
#'
#'

# ooshirabiso live_standinddead ####
#' Make 4 dbh histgram of A. mariesii with live_standinddead
#'
#' @return
#' @export
#'
#' @examples
#' Fig_live_standinddead()
Fig_live_standinddead<-function(){
  plt.<-c("Kaminokodaira","Matsuotoge","Mimatsu","Kagamiishi")
  cls<-seq(10,110,10)
  par(mfrow=c(2,2))
  for(ii in 1:length(plt.)){ #ii<-1
    d.<-subset(dd3,plot==plt.[ii]  & sp=="ooshirabiso" & !is.na(d01))
    dbh.<-d.$d01
    f.<- d.$f01
    dbh.<-dbh.[dbh.>10]
    dbh.cls<-cut(dbh.,cls)
    dbh.cls.live <- dbh.cls[f.>0]
    dbh.cls.standingdead <- dbh.cls[f.==0]

    live_dead<-rbind(table(dbh.cls.live ),table(dbh.cls.standingdead  ))
    barplot(live_dead,main=plt.[ii])
  }

  legend(5,30,c("Live","Standing dead"),pch=c(15,22),col=c("black","gray"))

}



#' Fig_live_standinddead_ratio
#'
#' @return
#' @export
#'
#' @examples
#' Fig_live_standinddead_ratio()
Fig_live_standinddead_ratio<-function(){
  plt.<-c("Kaminokodaira","Mimatsu","Matsuotoge","Kagamiishi")
  standinddead_ratio<-c()

  for(ii in 1:length(plt.)){ #ii<-2
    d.<-subset(dd3,plot==plt.[ii]  & sp=="ooshirabiso" & !is.na(d01) & d01>10) #
    dbh.<-d.$d01
    f.<- d.$f01
    ba.<-pi*(dbh./200)^2
    ba.live <- sum(ba.[f.>0])
    #c(nrow(d.),sapply(list(dbh.,ba.,ba.live),length))
    ba.standingdead <- sum(ba.[f.==0 | is.na(f.)])
    standinddead_ratio<-c(standinddead_ratio,ba.standingdead/( ba.live+ba.standingdead))
  }
  par(mfrow=c(1,1))
  barplot(standinddead_ratio,name=plt.,main="Abies mariesii",ylab="Standing Dead Ratio")
  legend(5,30,c("Live","Standing dead"),pch=c(15,22),col=c("black","gray"))

}



library(dplyr)
nasa
data("nasa", package = "dplyr")
nasa

library(ggplot2)
data(package="ggplot2")
?diamonds

data(package="TateyamaForest")
ls()
dir("data/")
data(load("data/TateyamaForest_dd3_plt.RData"))
## restore the saved values to the current environment
local({
  load("data/TateyamaForest_dd3_plt.RData")
  ls()
})

library(fs)
dir_tree()

data(TateyamaForest_dd3_plt)
data()
rm(list=ls())
ls()
data()
data(package="raster")

data(package="nenrin")
data(package="RGBFisheye")
data(package="ForestTools")
data(package="TateyamaForest")
library(TateyamaForest)
dd2

library(sf)
demo(package = "sf")

URL.CURRENT <- "http://api.openweathermap.org/data/2.5/weather?"
current.Weather <- getURL(paste0(URL.CURRENT,"q=","Tokyo"))
fromJSON(current.Weather);

z<-c("a","b","g")
for(i in 1:length(z))assign(z[i],1:10)
g
letters
a<-tibble(tibble(x = 1:3, y = list(1:5, 1:10, 1:20)))
          #> # A tibble: 3 × 2)
a
str(a)
a$y[1]



library(tidyverse)
library(gtrendsR)
library(rvest)
library(ngramr)

trend <- gtrends(keyword = "コロナ", geo = "JP")
plot(trend)

trend <- gtrends(keyword = c("ロシア", "ウクライナ"), geo = "JP")
plot(trend)

#
d<-subset(dd3,plot==6)
plot(d$x,d$y)

