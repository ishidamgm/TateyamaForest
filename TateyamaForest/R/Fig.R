# Fig.R

#'　library("TateyamaForest")
#'  help(package="TateyamaForest")
#'  data(package="TateyamaForest")
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


#' Calculates basal areas of each period
#'
#' omit tree data with is.na(dbh) and dbh < 10cm
#'
#' @param d   forest data.
#' @param sp  Roman species name. if default is sp="", that operate for all species
#'
#' @return　vector of sum of basal area for each period(year)
#' @export
#'
#' @examples
#' plt
#' plot.<-"Matsuotoge"
#' d<-subset(dd3,plot==plot.) #d<-subset(dd3.,pn==6) #
#' BA_calc(d,"オオシラビソ")
#' (ba.<-BA_calc(d,""))
#' ba./ba.[1]
BA_calc.old <-function(d,sp=""){　#sp="buna"
  i<-if(sp==""){1:nrow(d)}else{d$sp==sp} # 種の指定が""の時全種
  f<-d[i,clm_f]
  dbh<- d[i,clm_dbh]
  dbh[dbh<10 | f<1 ]<-0
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
#'  (BA0 <- BA_matrix2(sp.,dbh.min=0))
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
#' (BA0 <- BA_matrix2(sp.,dbh.min=0))
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
#' BA_calc(d,"sugi")
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
BA_calc <-function(d,sp="",dbh.min=10,f.min=1){　#sp="buna"
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
#'      xlim=c(0.25,6) , ylim=c(rng[1]-.1,rng[2]+.02),
#'      xlab="period",ylab="Basal area ratio",main=sp.)
#' abline(h=1)
#' plr <- which(BA[,1]>0) # plr : plot.recorded
#' for (ii in plr)lines(BAr[ii,],type="b",
#'                      lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
#' legend(0.2,0.95,leg$n,pch=leg$pch,col=leg$col,lty=leg$lty,cex=0.7)
#'
#'
#'
#' # "ooshirabiso" ####
#' sp.<- "オオシラビソ"
#' (BA <- BA_matrix2(sp.))
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
#'      xlim=c(1,6) , ylim=c(rng[1]-.1,rng[2]+.1),
#'      xlab="period",ylab="Basal area ratio",main=sp.)
#' abline(h=1)
#' plr <- which(BA[,1]>0) # plr : plot.recorded
#' for (ii in plr)lines(BAr[ii,],type="b",
#'                      lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
#'  i<-4:7
#' legend(1,0.90,leg$n[i],pch=leg$pch[i],col=leg$col[i],lty=leg$lty[i],cex=0.7)
#'
BA_matrix <- function(sp="オオシラビソ"){
  BA <- matrix(0,8,6)
  for (ii in 1:8){
    d<-subset(dd3,pn==ii)
    BA[ii,] <- BA_calc(d,sp)
  }
  return(BA)
}


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
#' sp.1 <- "オオシラビソ"
#' sp.2 <- "ブナ"
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
  d <- dd3[dd3$plot==plotname,]
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
#' vital_mortality(dd3)        # all species ####
#' vital_mortality(dd3,"スギ") #Cryptomeria japonica スギ　####
#' vital_mortality(dd3,"ブナ") # Fagus crenata ブナ　####
#' vital_mortality(dd3,"オオシラビソ") # Abies mariesii オオシラビソ　####
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
#' dbh_hist("Kaminokodaira","オオシラビソ",i, 10,seq(0,60,5),ylim=c(0,15))
#' }
#'
#' for(i in 1:6){
#' dbh_hist("Kagamiishi","オオシラビソ",i,0,seq(0,30,2),ylim=c(0,40))
#' }
#'
dbh_hist <- function(plotname="Kaminokodaira",species="オオシラビソ",term=1,
                     dbh_min=10,breaks=seq(0,120,10),...){
  ii<-which(plt$na==plotname)
  d <- dd3[dd3$plot==plotname,]
  sp <- d$sp
  yrc<-match(paste0("yr",1:6),names(plt))
  dbhc<-match(paste0("d0",1:6),names(d))
  fc<-match(paste0("f0",1:6),names(d))
  dbh. <- d[,dbhc[term]]
  f.   <- d[,fc[term]]

  Year <- plt[ii,yrc[term]]

  i <- sp==species & !is.na(dbh.) & dbh.>=dbh_min
  j <- f.>0

  h.all  <- hist(dbh. [i],col="black",breaks=breaks,
                 xlab="DBH (cm)", main=paste(plotname,Year, species,sep="_"),...)
  h.dead <- hist(dbh. [i & f.>0],breaks=breaks,col="white",add=T)

}

#' Make 4 dbh histgram of A. mariesii with live_standingdead
#'
#' @return
#' @export
#'
#' @examples
#' Fig_live_standingdead()
#'  legend(5,30,c("Standing dead","Living"),col=c("gray","black"),pch=c(15,15))

Fig_live_standingdead<-function(){
  plt.<-c("Kaminokodaira","Matsuotoge","Mimatsu","Kagamiishi")
  cls<-seq(10,110,10)
  par(mfrow=c(2,2))
  for(ii in 1:length(plt.)){ #ii<-1
    d.<-subset(dd3,plot==plt.[ii]  & sp=="オオシラビソ" & !is.na(d01))
    dbh.<-d.$d01
    f.<- d.$f01
    dbh.<-dbh.[dbh.>10]
    dbh.cls<-cut(dbh.,cls)
    dbh.cls.live <- dbh.cls[f.>0]
    dbh.cls.standingdead <- dbh.cls[f.==0]

    live_dead<-rbind(table(dbh.cls.live ),table(dbh.cls.standingdead  ))
    barplot(live_dead,main=plt.[ii])

  }
}
  #' Fig_live_standingdead_ratio
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' Fig_live_standingdead_ratio()
  Fig_live_standingdead_ratio<-function(){
    plt.<-c("Kaminokodaira","Mimatsu","Matsuotoge","Kagamiishi")
    standingdead_ratio<-c()

    for(ii in 1:length(plt.)){ #ii<-2
      d.<-subset(dd3,plot==plt.[ii]  & sp=="オオシラビソ" & !is.na(d01) & d01>10) #
      dbh.<-d.$d01
      f.<- d.$f01
      ba.<-pi*(dbh./200)^2
      ba.live <- sum(ba.[f.>0])
      #c(nrow(d.),sapply(list(dbh.,ba.,ba.live),length))
      ba.standingdead <- sum(ba.[f.==0 | is.na(f.)])
      standingdead_ratio<-c(standingdead_ratio,ba.standingdead/( ba.live+ba.standingdead))
    }
    par(mfrow=c(1,1))
    barplot(standingdead_ratio,name=plt.,main="Abies mariesii",ylab="Standing Dead Ratio(Basal area)")
    legend(5,30,c("Live","Standing dead"),pch=c(15,22),col=c("black","gray"))

  }

  #' Title
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' Fig_cunsumDeathRatio()
  #'
  #'
  Fig_cunsumDeathRatio<-function(){
    cls.<-seq(0,110,5)
    plr<-4:7
    cunsumDeathRatio<-c()
    for(ii in plr){  #ii<-1
      d.<-subset(dd3,pn==ii  & sp=="オオシラビソ" & f01>0 & !is.na(d01))
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










