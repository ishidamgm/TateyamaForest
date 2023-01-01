# Fig.R

#' returns basal area of a plot for a species
#'
#' @param plotname
#' @param species
#'
#' @return data.frame(Year,BasaAera)
#' @export
#'
#' @examples
#' plot. <- "Kaminokodaira"
#' sp.1 <- "ooshirabiso"
#' sp.2 <- "buna"
#' ba.sp1 <- BasalArea_1_6(plot.,sp.1)
#' ba.sp2 <- BasalArea_1_6(plot.,sp.2)
#' Year <- ba.sp1$Year
#' rba.sp1 <- 100*ba.sp1[,2]/ba.sp1[1,2]   #relative basal area
#' rba.sp2 <- 100*ba.sp2[,2]/ba.sp2[1,2]   #relative basal area
#' plot(Year,rba.sp1,ylab="Basal area (%)", ylim=c(60,200),
#' type="b",lwd=5,pch=2,col="blue", main="Kaminokodaira (a.s.l. 1450m　ecotone)")
#' lines(Year,rba.sp2,type="b",col="orange",lwd=5)
#' abline(h=100,col="red",lty=2,lwd=2)
#' legend(2000,180,c("Abies mariesii","Fagus crenata"),lwd=5,pch=c(2,1),col=c("blue","orange"))
BasalArea_1_6 <- function(plotname="Kaminokodaira",species= "ooshirabiso"){
  ii<-which(plt$na==plotname)
  d <- dd3[dd3$plot==ii,]
  yrc<-match(paste0("yr",1:6),names(plt))
  dbhc<-match(paste0("d0",1:6),names(d))
  ba <- pi*(d[,dbhc]/200)^2
  sp_ = species
  i.sp <- d$sp == sp_
  ba_ <- ba[i.sp,]
  BasalAera <- as.vector(colSums(ba_,na.rm=T))
  Year <-as.numeric(plt[ii,yrc])
  return(data.frame(Year,BasalAera))
}

#' plot relationships vital index and mortality from term 2 to 6
#'
#' @param d
#'
#' @return matrix of mortality
#' @export
#'
#' @examples
#'
#' par(mfrow=c(1,4))
#' vital_mortality(dd2)        # all species ####
#' vital_mortality(dd2,"スギ") #Cryptomeria japonica スギ　####
#' vital_mortality(dd2,"ブナ") # Fagus crenata ブナ　####
#' vital_mortality(dd2,"オオシラビソ") # Abies mariesii オオシラビソ　####
#'

vital_mortality <- function(d.,sp.=""){# sp.="スギ" ;　d.=dd2

  if(sp.==""){d<-d.}else{d<-subset(d.,sp==sp.)}
  d[is.na(d)]<- -999

  mt<-matrix(0,4,5)
  colnames(mt)<-paste0("vital",1:5)
  rownames(mt)<-paste0("term",3:6)

  # 2期の活力度とその後の死亡率
  for (i in 3:6){ #i=6　　1期は欠測があるので除外
    f1<-d[,clm_f[2]] ; f2<-d[,clm_f[i]]
    t. <- table(f1,f2)
    f1.<- match(1:5,rownames(t.))  #2期に生存していた出現活力度
    f2.<- match(-1:5,colnames(t.))　#i期に倒伏枯死木も含めた出現活力度
    t.<-t.[f1.,f2.]
    t.[,which(is.na(colnames(t.)))]<-0
    t.[which(is.na(rownames(t.))),]<-0

    all.sum  <- rowSums(t.)
    live.sum <- rowSums(t.[,3:7])
    dead.sum <- rowSums(t.[,1:2])
    mt[i-2,] <- dead.sum/ all.sum

  }


  #plot(mt[1,],type="b",ylim=c(0,0.8),xlab="活力度",ylab="死亡率",col=2,lty=1,pch=2)
  if(sp.==""){lbl<-"全種"}else{lbl<-sp.}
  plot(0,type="n",main=lbl,
       xlim=c(1,5),ylim=c(0,1),xlab="活力度",ylab="term2からの死亡率")
  i12<-1:nrow(mt)
  for(i in i12)lines(mt[i,],type="b",col=i,lwd=2,lty=1,pch=i)
  legend(3,.7,rownames(mt),lty=1,pch=i12,col=i12,lwd=2)

  return(mt)

}

#' histgram of dbh including dead standing trees
#'
#' @param plotname
#' @param species
#' @param term
#'
#' @return
#' @export
#'
#' @examples
#' par(mfrow=c(2,3))
#' for(i in 1:6){
#' dbh_hist("Kaminokodaira","ooshirabiso", i,ylim=c(0,13))
#' }
#'
dbh_hist <- function(plotname="Kaminokodaira",species="ooshirabiso", term=1,...){
  ii<-which(plt$na==plotname)
  d <- dd3[dd3$plot==ii,]
  sp <- d$sp
  yrc<-match(paste0("yr",1:6),names(plt))
  dbhc<-match(paste0("d0",1:6),names(d))
  fc<-match(paste0("f0",1:6),names(d))
  dbh. <- d[,dbhc[term]]
  f.   <- d[,fc[term]]

  Year <- plt[ii,yrc[term]]

  i <- sp==species & !is.na(dbh.) & dbh.>=10
  j <- f.>0

  h.all  <- hist(dbh. [i],col="black",
                 xlab="DBH (cm)", main=paste(plotname,Year, species,sep="_"),...)
  h.dead <- hist(dbh. [i & f.>0],col="white",add=T)

}








