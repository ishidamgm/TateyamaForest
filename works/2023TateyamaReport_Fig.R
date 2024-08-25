#### 2023TateyamaReport_Fig.R

library(TateyamaForest)
setwd("./test")
load("立山毎木調査_dd_plt.Rdata")

###　Tateyama_DiametricSpecies_test.R ####

diam_sp1<-function(sp,dbh){
  Species <- unique(sp)
  SpeciesNumber <- length(Species)
  cls <- seq(0,110,5)					# 5cm刻みの直径階を作成
  ds <- table(sp,cut(dbh,cls))				#　各樹種の直径階別本数の一覧表)
  DiametricSpecies <- t(ds)
  DiametricSpeciesNumber <- length(ds[ds!=0])
  df<-data.frame(SpeciesNumber ,DiametricSpeciesNumber)
  return(list(Species=Species, DiametricSpecies=DiametricSpecies,df=df))
}
diam_sp<-function(sp,dbh,sn=100){
  d<-diam_sp1(sp,dbh)
  Species<-d$Species ; DiametricSpecies<-d$DiametricSpecies ;df <-d$df
  n <- length(sp)
  NNN <- c()
  SpeciesNumber.sum = 0
  DiametricSpeciesNumber.sum = 0
  for (iii in 1:sn){
    NN <- c()
    for (ii in 1:n){
      #i<-sample(n,sn,replace=TRUE)  #sample(n,ii)
      i<-sample(n,ii)  #sample(n,ii)
      NN<-rbind(NN,diam_sp1(sp[i],dbh[i])[[3]])
    }
    SpeciesNumber.sum<-SpeciesNumber.sum+NN$SpeciesNumber
    DiametricSpeciesNumber.sum<-DiametricSpeciesNumber.sum+NN$DiametricSpeciesNumber
  }
  SpeciesNumber=SpeciesNumber.sum/sn
  DiametricSpeciesNumber=DiametricSpeciesNumber.sum/sn
  NNN<-data.frame(SpeciesNumber,DiametricSpeciesNumber)
  return(list(sp=Species, DiametricSpecies=DiametricSpecies,N=df,NN=NNN))
}
if(0){
  sp<-dd[[1]]$sp ; dbh<-dd[[1]]$d01
  z<-diam_sp(sp,dbh,2000)$NN
  plot(z[,2],type="l")
  lines(z[,1],col="blue")
}
dset<-function(d,jj=1){
  #ii<-1
  #d<-dd[[ii]]
  d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col[jj]]))
  (dsp<-diam_sp(d_$sp,d_$dbh))
  return(dsp$NN)
}
dbh_col=c("d01","d02","d03","d04","d05","d06")
th_lbl=c("1st","2nd","3rd","4th","5th","6th")
fig_DiametricSpecies_6periods<-function(ii=1,N=1){
  d<-dd[[ii]]
  plot(0,type="n",col=1,xlim=c(0,700),ylim=c(0,90),
       xlab="Individuals",ylab="Diametric Species",
       main=plt$na[ii])
  for (jj in 1:6){
    d_<-na.omit(data.frame(sp=d$sp,dbh=d[,dbh_col[jj]]))
    print(nrow(d_))
    dsp<-diam_sp(d_$sp,d_$dbh,N)
    NN<-dsp$NN
    lines(NN[,2],type="l",col=jj)
  }
}


## N ; numver of  random samplings ####
#windows()
par(mfrow=c(3,3))
for(ii in (1:8)[-7])fig_DiametricSpecies_6periods(ii,N=2000)
plot(1:10,1:10,type="n",axes=F,xlab="",ylab="")
for (i in 1:6){
  lines(c(3,8),c(i+2,i+2),col=i)
  text(rep(1.5,6),i+2,th_lbl[i],col=i)
}


# BA gwwth ####
plt
plot.<-"Kaminokodaira"
d<-subset(dd3,plot==plot.)
# Vital check ####
(f.<-d[,c(clm_f)])
table(as.numeric(as.matrix(f.)))
sum(is.na(f.))
# all tree species ####
sp.<- ""
(BA <- BA_matrix(sp.))
# absolute ###
par(mfrow=c(1,1))
plot(0,type="n" ,xlim=c(1,6),ylim=c(0,90),
     xlab="Period",ylab="Basal Area (m*m)")
for (ii in 1:8)lines(BA[ii,],type="b",lty=leg$lty[ii],col=leg$col[ii],pch=leg$pch[ii])
# ratio ####
(BAr <- BA/BA[,1])
rng<-range(BAr,na.rm=TRUE)
plot(0,type="n" , lty=ii,pch=ii,col=ii,
     xlim=c(0.25,6) , ylim=c(rng[1]-.02,rng[2]+.02),
     xlab="period",ylab="Basal area ratio",main=sp.)
abline(h=1)
plr <- which(BA[,1]>0) # plr : plot.recorded
for (ii in plr)lines(BAr[ii,],type="b",
                     lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
legend(0.5,1.2,leg$n,pch=leg$pch,col=leg$col,lty=leg$lty,cex=0.7)

# Fig_cunsumDeathRatio Abies ####
Fig_cunsumDeathRatio()

# Kaminokodaira  Fagus - Abies ####
plot. <- "Kaminokodaira"
sp.1 <- "ooshirabiso"
sp.2 <- "buna"
ba.sp1 <- BasalArea_1_6(plot.,sp.1)
ba.sp2 <- BasalArea_1_6(plot.,sp.2)
Year <- ba.sp1$Year
rba.sp1 <- 100*ba.sp1[,2]/ba.sp1[1,2]   #relative basal area
rba.sp2 <- 100*ba.sp2[,2]/ba.sp2[1,2]   #relative basal area
plot(Year,rba.sp1,ylab="Basal area (%)", ylim=c(60,200),
     type="b",lwd=5,pch=2,col="blue", main="Kaminokodaira (a.s.l. 1450m　ecotone)")
lines(Year,rba.sp2,type="b",col="orange",lwd=5)
abline(h=100,col="red",lty=2,lwd=2)
legend(2000,180,c("Abies mariesii","Fagus crenata"),lwd=5,pch=c(2,1),col=c("blue","orange"))

# Kaminokodaira_ooshirabiso_hist ####
par(mfrow=c(2,3))
for(i in 1:6){
  dbh_hist("Kaminokodaira","ooshirabiso",i, 10,seq(0,60,5),ylim=c(0,15))
}

# Fig_live_standingdead ####
Fig_live_standingdead()
legend(5,30,c("Standing dead","Living"),col=c("gray","black"),pch=c(15,15))




#'　library("TateyamaForest")
#'  help(package="TateyamaForest")
#'
#'

#' Calculates basal areas of each period
#'
#' omit tree data with is.na(dbh) and dbh < 10cm
#'
#' @param d   forest data.
#' @param sp  Roman species name. if default is sp="", that operate for all species
#'
#' @param dbh.min  a minimum value of DBH for calculation (default = 10)
#'
#' @param f.min  a minimum value of vital index for calculation  (default = 1)
#'
#'
#' @return　vector of sum of basal area for each period(year)
#' @export
#'
#' @examples
#' plt
#' plot.<-"Matsuotoge"
#' d<-subset(dd3,plot==plot.) #d<-subset(dd3.,pn==6) #
#' BA_calc(d,sp="オオシラビソ")
#' (ba.<-BA_calc(d,""))
#' ba./ba.[1]
#' plot.<-"Mimatsu"
#' d<-subset(dd3,plot==plot.)
#' (ba.<-BA_calc(d,"",dbh.min=0))
#'  (ba.<-BA_calc(d,"",dbh.min=10))
#' plot.<-"Kagamiishi"
#' d<-subset(dd3,plot==plot.)
#' plot(ba.<-BA_calc(d,"",dbh.min=0),type="b",ylim=c(0.4,1),main=plot.)
#' lines(ba..<-BA_calc(d,"",dbh.min=10),col="red",type="b")
#' legend(1.5,1.15,c("dbh>0cm","dbh>10cm"),col=c("black","red"),lty=c(1,1))
#'
#' plot(ba./ba.[1],type="b",ylim=c(0.9,1.2),main=plot.)
#' lines(ba../ba..[1],type="b",col="red")
#' legend(1.5,1.15,c("dbh>0cm","dbh>10cm"),col=c("black","red"),lty=c(1,1))
#'
BA_calc2 <-function(d,sp="",dbh.min=10,f.min=1){　#sp="buna"
  i<-if(sp==""){1:nrow(d)}else{d$sp==sp} # 種の指定が""の時全種
  f<-d[i,clm_f]
  dbh<- d[i,clm_dbh]
  dbh[dbh<dbh.min | f<f.min ]<-0
  ba <- pi*(dbh/200)^2
  ba.sum<-colSums(ba,na.rm=TRUE)
  return(ba.sum)
}



#' This makes Basal area matrix for 6 periods of tree species or All species
#'
#' @param d   data frame of forest stand trees
#' @param sp
#'
#' @param dbh.min  a minimum value of DBH for calculation (default = 10)
#'
#' @param f.min  a minimum value of vital index for calculation  (default = 1)
#'
#' @return
#' @export
#'
#' @examples
#' plt
#' plot.<-"Kaminokodaira"
#' d<-subset(dd3,plot==plot.)
#'
#' # Vital check ####
#' (f.<-d[,c(clm_f)])
#' table(as.numeric(as.matrix(f.)))
#' sum(is.na(f.))
#'
#'
#' # all tree species ####
#' sp.<- ""
#' (BA <- BA_matrix(sp.))
#'  (BA0 <- BA_matrix(sp.,dbh.min=0))
#' # Kagamiishi trees with more 1.3m height, others trees with more 10cm dbh
#' BA[7,]<-BA0[7,]
#'
#' # absolute ###
#' par(mfrow=c(1,1))
#' plot(0,type="n" ,xlim=c(1,6),ylim=c(0,90),
#' xlab="Period",ylab="Basal Area (m*m)")
#' for (ii in 1:8)lines(BA[ii,],type="b",lty=leg$lty[ii],col=leg$col[ii],pch=leg$pch[ii])
#'
#' # ratio ####
#' (BAr <- BA/BA[,1])
#' rng<-range(BAr,na.rm=TRUE)
#' plot(0,type="n" , lty=ii,pch=ii,col=ii,
#'      xlim=c(0.25,6) , ylim=c(rng[1]-.02,rng[2]+.02),
#'      xlab="period",ylab="Basal area ratio",main=sp.)
#' abline(h=1)
#' plr <- which(BA[,1]>0) # plr : plot.recorded
#' for (ii in plr)lines(BAr[ii,],type="b",
#'                      lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
#' legend(1.2,1.2,leg$n,pch=leg$pch,col=leg$col,lty=leg$lty,cex=0.7)
#'
#'
#'
#' # "ooshirabiso" ####
#' sp.<- "オオシラビソ"
#' (BA <- BA_matrix(sp.))
#' (BA0 <- BA_matrix(sp.,dbh.min=0))
#' #Kagamiishi trees with more 1.3m height, others trees with more 10cm dbh
#' BA[7,]<-BA0[7,]
#' # absolute ###
#' par(mfrow=c(1,1))
#' plot(0,type="n" ,xlim=c(1,6),ylim=c(0,20),
#' xlab="Period",ylab="Basal Area (m*m)")
#' for (ii in 1:8)lines(BA[ii,],type="b",lty=leg$lty[ii],col=leg$col[ii],pch=leg$pch[ii])
#'
#' # ratio ####
#' (BAr <- BA/BA[,1])
#' rng<-range(BAr,na.rm=TRUE)
#' plot(0,type="n" , lty=ii,pch=ii,col=ii,
#'      xlim=c(1,6) , ylim=c(rng[1]-.01,rng[2]+.01),
#'      xlab="period",ylab="Basal area ratio",main=sp.)
#' abline(h=1)
#' plr <- which(BA[,1]>0) # plr : plot.recorded
#' for (ii in plr)lines(BAr[ii,],type="b",
#'                      lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
#'  i<-4:7
#' legend(1,rng[2]+.01,leg$n[i],pch=leg$pch[i],col=leg$col[i],lty=leg$lty[i],cex=0.7)
#'
#'
BA_matrix2 <- function(sp="オオシラビソ",dbh.min=10,f.min=1){
  BA <- matrix(0,8,6)
  for (ii in 1:8){
    d<-subset(dd3,pn==ii)
    BA[ii,] <- BA_calc(d,sp,dbh.min=dbh.min,f.min=f.min)
  }
  return(BA)
}
