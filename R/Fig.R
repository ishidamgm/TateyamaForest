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
#' plot.<-"Kaminokodaira"
#' d<-subset(dd3,plot==plot.) #d<-subset(dd3.,pn==6) #
#' BA_calc(d,"スギ")
#' (ba.<-BA_calc(d,""))
#' ba./ba.[1]
#' plot.<-"Mimatsu"
#' d<-subset(dd3,plot==plot.)
#' (ba.<-BA_calc(d,"",dbh.min=0))
#'  (ba.<-BA_calc(d,"",dbh.min=10))
#' plot.<-"Kagamiishi"
#' d<-subset(dd3,plot==plot.)
#' plot(ba.<-BA_calc(d,"",dbh.min=0),type="b",ylim=c(0.4,1),main=plot.,ylab="Basal Area")
#' lines(ba..<-BA_calc(d,"",dbh.min=10),col="red",type="b")
#' legend(1.5,1.0,c("dbh>0cm","dbh>10cm"),col=c("black","red"),lty=c(1,1))
#'
#' plot(ba./ba.[1],type="b",ylim=c(0.9,1.2),main=plot.)
#' lines(ba../ba..[1],type="b",col="red")
#' legend(1.5,1.15,c("dbh>0cm","dbh>10cm"),col=c("black","red"),lty=c(1,1))
#'
BA_calc <-function(d,sp="スギ",dbh.min=10,f.min=1){　#sp="buna"
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


  #' Fig_cunsumDeathRatio_2024
  #'
  #' @return cunsumDeathRatio
  #'
  #' @export
  #'
  #' @examples
  #' Fig_cunsumDeathRatio_2024()
  #'
  #'
  Fig_cunsumDeathRatio_2024<-function(sp.="オオシラビソ",plot_name=c("Kaminokodaira","Matsuotoge","Kagamiishi"),main="Abies mariesii (1999-2024)"){
    cls.<-seq(0,65,5)
    cls.n <- length(cls.)
    cunsumDeathRatio<-c()
    dbh.max <-c()
    plr <- match(plot_name,plt$na)
    for(ii in plr){  #ii<-4
      d.<-subset(dd4,pn==ii & sp==sp. & f01>0 & !is.na(d01))
      (f.<-d.[,c(clm_f)])
      dbh.<-d.$d01
      dbh.max <-c(dbh.max ,max( dbh.))
      dbh.cls<-cut(dbh.,cls.)
      dbh.cls.dead<-dbh.cls[d.$f07<1]
      cunsumDeathRatio.<-cumsum(table(dbh.cls.dead))/nrow(d.)
      cunsumDeathRatio<-c(cunsumDeathRatio,list(cunsumDeathRatio.))
    }

    # plot

    plot(0,type="n" ,
         xlim=c(0,65) , ylim=c(0,0.60),
         xlab="DBH (cm)",ylab="Mortality ratio",cex.lab=1.2,cex.axis=1.1,main=main) #,main="A. mariesii (term 1-7)"

    dbh.cls.max <-as.numeric(cut(dbh.max,cls.))+1

    for (ii in 1:length(cunsumDeathRatio)){
      jj<-plr[ii]
      lines(cls.[1:dbh.cls.max[ii]],cunsumDeathRatio[[ii]][1:dbh.cls.max[ii]],type="b",
            lty=leg$lty[jj],pch=leg$pch[jj],col=leg$col[jj])
    }

    #legend(5,0.55,leg$n[plr],pch=leg$pch[plr],col=leg$col[plr],lty=leg$lty[plr],cex=1)
    legend(0,0.58,c("Ecotone plot","Subarctic plot","Timberline plot"),
           pch=leg$pch[plr],col=leg$col[plr],lty=leg$lty[plr],cex=1)
    return(cunsumDeathRatio)
    #test
    # d.<-subset(dd4,plot=="Kaminokodaira"  & sp=="オオシラビソ" & f01>0 & !is.na(d01))　# nrow(d.)
    # nrow(d.[d.$f07<1,]) /nrow(d.[d.$f01>0 & !is.na(d.$d01), ])
    #"Kaminokodaira"　0.5,　"Matsuotoge"　0.1845238　,"Kagamiishi"　0.152
  }


  #' オオシラビソの立枯木が倒伏する割合
  #'
  #' @param d.    a data.frame. The default is subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ")
  #' @param form  a vector of character for form survey period, default is form=c("f03","f04")
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' # 立枯木の倒伏率の経年変化
  #'
  #' fallen_ratio()
  #' fallen_ratio(form="f07")
  #' fallen_ratio(form=paste0("f0",1:7))
  #' years<-as.numeric(subset(plt2,na=="Kaminokodaira",(paste0("yr",1:7))))
  #' plot( years,fallen_ratio(form=paste0("f0",1:7)),type="b",
  #' ylab="Falling ratio of standing dead trees",xlab="Year",main="A.mariesii in Kaminokodaira")
  #'
  #'
  fallen_ratio<-function(d.= subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ"),form=c("f03","f04")){
    ratio.<-c()
    for(i in form){
      ratio.<-c(ratio.,sum(d.$f01==0 & d.[,i]==-1,na.rm=T)/sum(d.$f01==0,na.rm=T) )
    }
    return(ratio.)
  }

#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
#' Fig_fallen_ratio()
#'
Fig_fallen_ratio <- function(){
  d. <- subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ")
  # fallen_ratio()
  # fallen_ratio("f07")
  years<-as.numeric(subset(plt2,na=="Kaminokodaira",(paste0("yr",1:7))))
  #df <- data.frame(Year=years,Fallen_ratio=fallen_ratio(form=paste0("f0",1:7)))
  df <- data.frame(Year=years,Fallen_ratio=fallen_ratio(form=c("f01","f02","f03","f04","f05","f06","f07")))
  plot( df,
        xlim=c(2000,2026),
        type="b",lwd=1.5,cex.lab=1.2,
  ylab="Falling ratio of dead standing trees",xlab="Year",main="Abies mariesii in Kaminokodaira")
  return(df)
}




#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
#' Fig_fallen_ratio2()
#'
Fig_fallen_ratio2 <- function(){
  d. <- subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ")
  years<-as.numeric(subset(plt2,na=="Kaminokodaira",(paste0("yr",1:7))))
  f0.<-c("f01","f02","f03","f04","f05","f06","f07")
  df <- data.frame(Year=years,Fallen_ratio=fallen_ratio(form=f0.)) #=paste0("f0",1:7)

  # '--------------------------

  i.<-d.$f01==0 & d.$f07==-1 & !is.na(d.$d01) # sum(i.)
  d.[which(i.),]
  sum(i.,na.rm=T)/sum(d.$f01==0,na.rm=T) # 初回立枯木25本のうち88%の22本が7期までに倒れた
  dbh.<-d.$d01[i.]
  (mtf.<-MTF(dbh.,6.2)) #枯れながら立っていた時間   平均気温　6.2℃
  (period.<-apply(d.[i.,paste0("f0",1:7)]==-1,1,function(x)which(x)[1]))
  #　何年に枯れたのか
  (yr_fall.<-Fplt_yr_median("Kaminokodaira",period.)) #倒れた年
  (yr_died. <- as.numeric(yr_fall.-mtf.))                        #枯れた年
  h<-hist(yr_died.)
  #' plot(h$mids,h$counts,type="h",lwd=10)
  # '--------------------------
  yr.<- c(1972.5,1977.5, 1982.5, 1987.5, 1992.5, 1997.5, 2002.5, 2007.5,2012.5)
  # polygon(data.frame(yr.,1-c(0,h$counts/sum(h$counts),0)),col="red",density=20)
  # text(2015,0.3,"Fall",cex=2)
  # rect(1985.5,0.75,1995,0.65,col="white",border="white") ; text(1991,0.8,"Death",cex=2,col="red")


  # Plot the main graph
  par(mar = c(5, 5, 4, 5))
  plot(df,
       xlim = c(1972, 2025), ylim = c(0, 1),
       type = "b", lwd = 1.5, cex.lab = 1.2,
       ylab = "Fall ratio of dead standing trees", xlab = "Year",
       main = "Abies mariesii in the Ecotone plot")

  # Add the polygon representing the estimated death period
  polygon(data.frame(yr.,1-c(0,h$counts/sum(h$counts),0)),col="red",density=20)

  # Label the fall phase
  text(2015, 0.3, "Fall", cex = 1.5)

  # Add a white rectangle to clear space for text
  rect(1985.5, 0.80, 1995, 0.90, col = "white", border = "white")
  text(1990.5, 0.85, "Death", cex = 1.5, col = "red")

  # Add the secondary y-axis on the right side (inverted)
  axis(4, at = seq(0, 1, 0.2), labels = rev(seq(0, 1, 0.2)), las = 1)
  mtext("Estimated frequency of death", side = 4, line = 3,col="red")



}

#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
#' Fig_fallen_ratio_bar()
#'
Fig_fallen_ratio_bar <- function(){
  d. <- subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ")
  years<-as.numeric(subset(plt2,na=="Kaminokodaira",(paste0("yr",1:7))))
  f0.<-c("f01","f02","f03","f04","f05","f06","f07")
  df <- data.frame(Year=years,Fallen_ratio=fallen_ratio(form=f0.)) #=paste0("f0",1:7)

  # '--------------------------

  i.<-d.$f01==0 & d.$f07==-1 & !is.na(d.$d01) # sum(i.)
  d.[which(i.),]
  sum(i.,na.rm=T)/sum(d.$f01==0,na.rm=T) # 初回立枯木25本のうち88%の22本が7期までに倒れた
  dbh.<-d.$d01[i.]
  (mtf.<-MTF(dbh.,6.2)) #枯れながら立っていた時間   平均気温　6.2℃
  (period.<-apply(d.[i.,paste0("f0",1:7)]==-1,1,function(x)which(x)[1]))
  #　何年に枯れたのか
  (yr_fall.<-Fplt_yr_median("Kaminokodaira",period.)) #倒れた年
  (yr_died. <- as.numeric(yr_fall.-mtf.))                        #枯れた年
  h<-hist(yr_died.)
  #' plot(h$mids,h$counts,type="h",lwd=10)
  # '--------------------------
  yr.<- c(1972.5,1977.5, 1982.5, 1987.5, 1992.5, 1997.5, 2002.5, 2007.5,2012.5)
  # polygon(data.frame(yr.,1-c(0,h$counts/sum(h$counts),0)),col="red",density=20)
  # text(2015,0.3,"Fall",cex=2)
  # rect(1985.5,0.75,1995,0.65,col="white",border="white") ; text(1991,0.8,"Death",cex=2,col="red")


  # Plot the main graph
  par(mar = c(5, 5, 4, 5))
  plot(df,
       xlim = c(1972, 2025), ylim = c(0, 1),
       type = "b", lwd = 1.5, cex.lab = 1.2,
       ylab = "Fall ratio of dead standing trees", xlab = "Year",
       main = "Abies mariesii in the Ecotone plot")

  # Add the polygon representing the estimated death period

  #polygon(data.frame(yr.,1-c(0,h$counts/sum(h$counts),0)),col="red",density=20)
  r<-data.frame(yr=yr.,freq=1-c(0,h$counts/sum(h$counts),0))
  rect(r$yr - 2.5, r$freq, r$yr + 2.5, 1, col="red", border="black")

  # Label the fall phase
  text(2015, 0.3, "Fall", cex = 1.5)

  # Add a white rectangle to clear space for text
 # rect(1985.5, 0.80, 1995, 0.90, col = "white", border = "white")
  text(1990.5, 0.55, "Death", cex = 1.5, col = "red")

  # Add the secondary y-axis on the right side (inverted)
  axis(4, at = seq(0, 1, 0.2), labels = rev(seq(0, 1, 0.2)), las = 1)
  mtext("Estimated frequency of death", side = 4, line = 3,col="red")



}



#' MTF: Mean Time to Fall of Dead Standing Trees
#' Gärtner et al. (2023).
#' Temperature and Tree Size Explain the Mean Time to Fall of Dead Standing Trees across Large Scales.
#  Forests, 14(5), 1017. https://doi.org/10.3390/f14051017
#
#' @param DBH
#' @param MAT mean annual temperature
#'
#' @return Mean Time to Fall of Dead Standing Trees
#'
#' @export
#'
#' @examples
#' site.<-c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi" )
#' leg$col[match(site.,leg$n)]
#' col.<-c("darkolivegreen3","blueviolet","blue","cyan3" )
#' mat.<-c(8.0,6.2,2.8,1.0)
#' dbh.<-1:60
#' plot(dbh.,MTF(dbh.,mat.[1]),type="l",lty=1,col=col.[1],
#' xlab="DBH (cm)",ylab="Mean Time to Fall of Dead Standing Trees")
#' for(i in 2:4)lines(dbh.,MTF(dbh.,mat.[i]),col=col.[i],lty=i)
#'  legend(2,47,site.,col=col.,lwd=2,lty=1:4)
#'
#'
#'
MTF <- function(DBH=30,MAT=5){
  exp(2.40+0.04*DBH-0.12*MAT)
}



#' Estimate death year of Abies using MTF (Mean Time to Fall) fall
#'
#' @param site
#' @param period
#'
#' @return
#' @export
#'
#' @examples
#' names(dd4)
#' d. <- subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ" & !is.na(d01))
#' i.<-d.$f01==0 & d.$f07==-1
#' sum(i.,na.rm=T)/sum(d.$f01==0,na.rm=T) # 初回立枯木25本のうち88%の22本が7期までに倒れた
#' dbh.<-d.$d01[i.]
#' (mtf.<-MTF(dbh.,6.2)) #枯れながら立っていた時間
#' (period.<-apply(d.[i.,paste0("f0",1:7)]==-1,1,function(x)which(x)[1]))
#' #　何年に枯れたのか
#' (yr_fall.<-Fplt_yr_median("Kaminokodaira",period.)) #倒れた年
#' (yr_died. <- as.numeric(yr_fall.-mtf.))                        #枯れた年
#' h<-hist(yr_died.)
#' summary(yr_died.)
#' .<-yr_died.
#' m.<-round(mean(.),1)
#' sd.<-round(sd(.),1)
#' "The death years of dead standing Abies mariesii trees in 2000 at the Kaminokodaira plot were estimated to be 1990.9 ± 5.7 (mean ± s.d.) "
#' "using the MTF model (Gärtner et al., 2023), and their frequency distribution fitted a normal distribution (Kolmogorov-Smirnov test: D = 0.129, p = 0.811)."
#' paste(m.,"±", sd.,"(mean ± s.d.)")
#' sum(mean(.)-2*sd(.)<. & .<mean(.)+2*sd(.))/length(.)
#' ks.test(x=yr_died.,y="pnorm",mean=mean(yr_died.),sd=sd(yr_died.))
#'
#'
#' df <- data.frame(Year=years,Fallen_ratio=fallen_ratio(form=paste0("f0",1:7)))
#' plot( df,
#'       xlim=c(1972,2025),ylim=c(0,1),
#'       type="b",lwd=1.5,cex.lab=1.2,
#'       ylab="Falling ratio of dead standing trees",xlab="Year",main="Abies mariesii in Kaminokodaira",add=T)
#'
#' polygon(data.frame(c(1972.5,h$mids,2012.5),1-0.88*c(0,10*h$density,0)),col="red",density=20)
#' text(2015,0.3,"Fall",cex=2)
#' rect(1987,0.8,1993,0.6,col="white",border="white") ; text(1990,0.7,"Death",cex=2,col="red")

Fplt_yr_median <-function(site="Kaminokodaira",period=2:7){
  y2<-Fplt_yr(site,period)
  y1<-Fplt_yr(site,period-1)
  y1+(y2-y1)/2
}

#' Title
#'
#' @param site
#' @param period
#'
#' @return
#' @export
#'
#' @examples
Fplt_yr <-function(site="Kaminokodaira",period=1){subset(plt5,na==site)[,paste0("yr",period)]}


#' Title
#'
#' @param site
#' @param period
#'
#' @return
#' @export
#'
#' @examples
#' names(dd4)
#' d. <- subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ" & !is.na(d01))
#' i.<-d.$f01==0 & d.$f07==-1
#' sum(i.,na.rm=T)/sum(d.$f01==0,na.rm=T) # 初回立枯木25本のうち88%の22本が7期までに倒れた
#' dbh.<-d.$d01[i.]
#' (mtf.<-MTF(dbh.,6.2)) #枯れながら立っていた時間
#' summary(mtf.)
#' paste("n=",length(mtf.)," mean=",mean(mtf.),"sd=",sd(mtf.),"min=",min(mtf.),"max=",max(mtf.))
#' (period.<-apply(d.[i.,paste0("f0",1:7)]==-1,1,function(x)which(x)[1]))
#' #　何年に枯れたのか
#' (yr_fall.<-Fplt_yr_median("Kaminokodaira",period.)) #倒れた年
#' (yr_died. <- as.numeric(yr_fall.-mtf.))                        #枯れた年
#' hist(yr_died.)
#' summary(yr_died.)
#' .<-yr_died.
#' m.<-round(mean(.),1)
#' sd.<-round(sd(.),1)
#' "The death years of dead standing Abies mariesii trees in 2000 at the Kaminokodaira plot were estimated to be 1990.9 ± 5.7 (mean ± s.d.) "
#' "using the MTF model (Gärtner et al., 2023), and their frequency distribution fitted a normal distribution (Kolmogorov-Smirnov test: D = 0.129, p = 0.811)."
#' paste(m.,"±", sd.,"(mean ± s.d.)")
#' sum(mean(.)-2*sd(.)<. & .<mean(.)+2*sd(.))/length(.)
#' ks.test(x=yr_died.,y="pnorm",mean=mean(yr_died.),sd=sd(yr_died.))
#'
Fplt_yr_median <-function(site="Kaminokodaira",period=2:7){
  y2<-Fplt_yr(site,period)
  y1<-Fplt_yr(site,period-1)
  y1+(y2-y1)/2
}

#' Title
#'
#' @param form
#'
#' @return
#' @export
#'
#' @examples
#' death_ratio()
#' years<-as.numeric(subset(plt2,na=="Kaminokodaira",(paste0("yr",1:7))))
#'
#'  plot( years,death_ratio(data=subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ"),form=paste0("f0",1:7)),type="b",
#'  ylab="枯死率",xlab="西暦年",main="初回2000年に生存していたオオシラビソ")
#'
#'
death_ratio<-function(data=subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ"),form=paste0("f0",1:7)){
  #d. <- subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ")
  ratio.<-c()
  for(i in form){
    ratio.<-c(ratio.,sum(data$f01>0 & data[,i]<1,na.rm=T)/sum(data$f01>0,na.rm=T) )
  }
  return(ratio.)
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
#' Fig_Kaminokodaira_Abies_death_ratio()
Fig_Kaminokodaira_Abies_death_ratio<-function(){
  death_ratio()
  years<-as.numeric(subset(plt2,na=="Kaminokodaira",(paste0("yr",1:7))))

   plot( years,death_ratio(data=subset(dd4,plot=="Kaminokodaira" & sp=="オオシラビソ"),form=paste0("f0",1:7)),type="b",
         xlim=c(2000,2026),
         main="Abies mariesii in the Ecotone plot",
   ylab="Mortality ratio",xlab="Year",cex.lab=1.2,lwd=2)

}

#' Title
#'
#' @return
#' @export
#'
#' @examples
#' Fig_Abies_death_ratio()
#'
Fig_Abies_death_ratio<-function(){

  plt.<-c("Kaminokodaira","Matsuotoge","Kagamiishi")
  plr<-match(plt.,plt2$na)
  Abies_death_ratio<-c()
  for (ii in plt.){
    year<-as.numeric(subset(plt2,na==ii,(paste0("yr",1:7))))
    death.ratio<- death_ratio( data = subset(dd4, plot == ii & sp == "オオシラビソ"))
    Abies_death_ratio<-c(Abies_death_ratio, list(data.frame(year,death_ratio=death.ratio)))
  }
  names(Abies_death_ratio)<-plt.


  leg.<-c(4,5,7)
  plot(  Abies_death_ratio[[1]],
        type="b",
        xlim=c(1998,2026),
        pch=leg$pch[leg.[1]],col=leg$col[leg.[1]],lty=leg$lty[leg.[1]],
        main="Abies mariesii",
        ylab="Mortality ratio",xlab="Year",cex.lab=1.2,lwd=2)

  for (ii in 2:3){
    xy<-Abies_death_ratio[[ii]]
    lines( xy,pch=leg$pch[leg.[ii]],col=leg$col[leg.[ii]],lty=leg$lty[leg.[ii]],lwd=2)
    points(xy,pch=leg$pch[leg.[ii]],col=leg$col[leg.[ii]],cex=1.5)
  }



  legend(2000,0.45,c("Ecotone plot","Subarctic plot","Timberline plot"),
         pch=leg$pch[leg.],col=leg$col[leg.],lty=leg$lty[leg.],cex=1)

  return(list(Kaminokodaira=kami,Mmatuotoge=matu,Kagamiishi=kaga))

}


