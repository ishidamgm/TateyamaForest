# Fig2024.R

# RData ####
#' Species basal area for each plot
#'
#' save as "sp_ba.RData"
#'
#' @param cond  condition of vital index(f.), default is "f.>0"(living tree)
#'
#' @return list of species basal area for each plot
#' @export
#'
#' @examples
#' (sp_ba <- yr_sp_ba_site())
#' (sp_ba_StandigDead <- yr_sp_ba_site(cond=substitute(f.==0)))
#'
#' # save(sp_ba,file="data/sp_ba.RData")
#' # save(sp_ba_StandigDead,file="data/ssp_ba_StandigDead.RData")
#'
#'(sp_ba_ratio<-sapply(sp_ba,function(x)t(t(x)/rowSums(t(x)))))
#'(sp_ba_ratio_StandigDead<-sapply(sp_ba_StandigDead,function(x)t(t(x)/rowSums(t(x)))))
#'
#'# save(sp_ba_ratio,file="data/sp_ba_ratio.RData")
#'# save(sp_ba_ratio_StandigDead,file="data/sp_ba_ratio_StandigDead.RData")
#'
#'

yr_sp_ba_site <-function(cond=substitute(f.>0)){
  ## load data ####

  . <- TateyamaForest2024
  d0=.$d0;plt=.$plot_profile;cnD=.$colnames_D;cnf=.$colnames_f;yr=.$yr


  ## ii plot no.
  sp_ba <- c()
  for(ii in 1:nrow(plt)){ #ii<-4
    d <- d0[[ii]]
    yr. <- na.omit(as.numeric(yr[ii,]))
    cnD. <- na.omit(as.character(cnD[ii,]))
    cnf. <- na.omit(as.character(cnf[ii,]))
    dbh. <- d[,cnD.]
    f. <- d[,cnf.]
    # dbh.. <- dbh. * (dbh.>10)* (f.>0) # dbh more than 10cm, Vitality Index more than 1 (namely living)
    dbh.. <- dbh. * (dbh.>10)* eval(cond) # dbh more than 10cm, Vitality Index more than 1 (namely living)

    if(names(d0)[ii]=="Kagamiishi"){dbh.. <- dbh. * eval(cond)}  # calculated all trees for Kagamiishi site on timber line
    dbh..[is.na(dbh..)]<-0
    ba..<-pi*(dbh../200)^2
    #ba..<-data.frame(sp=d$sp,ba=ba..)

    sp_ba.<-c()
    for(i in 1:ncol(ba..)){
      sp_ba.<-cbind(sp_ba.,tapply(ba..[,i],d$sp,sum))
    }
    colnames(sp_ba.)<-na.omit(as.numeric(yr[ii,]))


    #ba. <- data.frame(Year=yr., BasalArea=colSums(pi*(dbh../200)^2,na.rm=TRUE))
    sp_ba <- c(sp_ba,list(sp_ba.))

  }
  names(sp_ba)<-plt$na

  return(sp_ba)
}


#' Return a list of data frame for year and basal area  per every site
#'
#' @return list of data frame for year and basal area  per every site
#' @export
#'
#' @examples
#' BA <- yr_ba_site(cond=substitute(f.>0))
#'  plot(0,type="n" , lty=1,pch=1,col=1,
#'      xlim=c(1998,2025) , ylim=c(0.97,1.23),
#'      xlab="Year",ylab="Basal area ratio")
#' abline(h=1)
#' for (ii in 1:length(BA)){
#'  . <- BA[[ii]]
#'  lines(.$Year,.$BasalArea/.$BasalArea[1],type="b",
#'                      lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])#'
#' }
#'
#' legend(2000,1.2,leg$n,pch=leg$pch,col=leg$col,lty=leg$lty,cex=0.7)
#'
#'
yr_ba_site <-function(cond=substitute(f.>0)){
  ## load data ####
  d0  <- TateyamaForest2024$d0
  plt <- TateyamaForest2024$plot_profile
  cnD <- TateyamaForest2024$colnames_D
  cnf <- TateyamaForest2024$colnames_f
  yr  <- TateyamaForest2024$yr

  # . <- TateyamaForest2024
  # plt=.$plot_profile;cnD=.$colnames_D;cnf=.$colnames_f;yr=.$yr


  ## ii plot no.
  ba <- c()
  for(ii in 1:nrow(plt)){
    d <- d0[[ii]]
    yr[ii,]
    yr. <- na.omit(as.numeric(yr[ii,]))
    cnD. <- na.omit(as.character(cnD[ii,]))
    cnf. <- na.omit(as.character(cnf[ii,]))
    dbh. <- d[,cnD.]
    f. <- d[,cnf.]
    dbh.. <- dbh. * (dbh.>10)* (f.>0) # dbh more than 10cm, Vitality Index more than 1 (namely living)
    if(names(d0)[ii]=="Kagamiishi"){dbh.. <- dbh. * (f.>0)}  # calculated all trees for Kagamiishi site on timber line
    ba. <- data.frame(Year=yr., BasalArea=colSums(pi*(dbh../200)^2,na.rm=TRUE))
    ba <- c(ba,list(ba.))

  }
  names(ba)<-plt$na

  return(ba)
}

#' Fig_yr_ba_site2
#'
#' @return nothing, draw a figure only.
#' @export
#'
#' @examples
#'
#' Fig_yr_ba_site2()
#'
#'
Fig_yr_ba_site2 <-function(){
  BA <- yr_ba_site()
   plot(0,type="n" , #lty=ii,pch=ii,col=ii,
       xlim=c(1998,2025) , ylim=c(0.97,1.23),
       xlab="Year",ylab="Ratio of total basal area")
  abline(h=1)
  n<-match(plt2$na,names(BA))
  for (ii in n){
   . <- BA[[ii]]
   lines(.$Year,.$BasalArea/.$BasalArea[1],type="b",
                       lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])#'
  }

  legend(1998,1.22,leg$n[n],pch=leg$pch[n],col=leg$col[n],lty=leg$lty[n],cex=0.8)
}

#' Fig_yr_ba_site2
#'
#' @return nothing, draw a figure only.
#' @export
#'
#' @examples
#'
#' Fig_yr_ba_site2_zone ()
#'
#'
Fig_yr_ba_site2_zone <-function(){
  BA <- yr_ba_site()
  plot(0,type="n" , #lty=ii,pch=ii,col=ii,
       xlim=c(1998,2025) , ylim=c(0.97,1.23),
       xlab="Year",ylab="Ratio of total basal area")
  abline(h=1)
  n<-match(plt2$na,names(BA))
  for (ii in n){
    . <- BA[[ii]]
    lines(.$Year,.$BasalArea/.$BasalArea[1],type="b",
          lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])#'
  }

  #legend(1998,1.22,leg$n[n],pch=leg$pch[n],col=leg$col[n],lty=leg$lty[n],cex=0.8)
  plot_name <-c("Temperate plot","Ecotone plot","Subarctic plot","Timberline plot")
  legend(1998,1.22,plot_name,pch=leg$pch[n],col=leg$col[n],lty=leg$lty[n],cex=0.8)
}


# Kaminokodaira  Fagus - Abies ####
#' Draw a figure relatinships between year and basal area ratio for Fagus - Abies in Kagamiishi in 2024
#'
#' @return nothing, only output figure
#' @export
#'
#' @examples
#' Fig_yr_ba_kaminokodaira_Fagus_Abies_2024()
Fig_yr_ba_kaminokodaira_Fagus_Abies_2024 <- function(){
  .<-sp_ba_ratio
  plot. <- "Kaminokodaira"
  sp.1 <- "オオシラビソ"
  sp.2 <- "ブナ"

  bar.<-.[[plot.]]
  Year <- as.numeric(colnames(bar.))
  rba.sp1 <- 100*bar.[sp.1,]/bar.[sp.1,1]   #relative basal area
  rba.sp2 <- 100*bar.[sp.2,]/bar.[sp.2,1]    #relative basal area
  plot(Year,rba.sp1,ylab="Basal area (%)", ylim=c(40,180),
       type="b",lwd=5,pch=2,col="blue", main="Kaminokodaira (a.s.l. 1450m　ecotone)")
  lines(Year,rba.sp2,type="b",col="orange",lwd=5)
  abline(h=100,col="red",lty=2,lwd=2)
  legend(2000,180,c("Abies mariesii","Fagus crenata"),lwd=5,pch=c(2,1),col=c("blue","orange"))

}

# Kaminokodaira  Cryptomeria - Fagus - Abies ####
#' Draw a figure relatinships between year and basal area ratio for Fagus - Abies in Kagamiishi in 2024
#'
#' @return nothing, only output figure
#' @export
#'
#' @examples
#' Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024()
Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024 <- function(){
  .<-sp_ba_ratio
  plot. <- "Kaminokodaira"
  sp.1 <- "オオシラビソ"
  sp.2 <- "ブナ"
  sp.3 <- "スギ"

  bar.<-.[[plot.]]
  Year <- as.numeric(colnames(bar.))
  rba.sp1 <-bar.[sp.1,]/bar.[sp.1,1]   #relative basal area
  rba.sp2 <- bar.[sp.2,]/bar.[sp.2,1]    #relative basal area
  rba.sp3 <- bar.[sp.3,]/bar.[sp.3,1]    #relative basal area
  plot(Year,rba.sp1,ylab="Ratio of total basal area", ylim=c(.4,1.8),
       type="b",lwd=3,pch=2,col="blue", main="In the Ecotone Plot",cex=1.2,cex.lab=1.4)
  lines(Year,rba.sp2,type="b",col="orange",lwd=3,cex=1.2)
  lines(Year,rba.sp3,type="b",col="red",lwd=3,pch=17,cex=1.2)
  abline(h=1,col="red",lty=2,lwd=2)
  legend(2000,1.8,c("Abies mariesii","Fagus crenata","Cryptomeria japonica"),pch=c(2,1,17),col=c("blue","orange","red"),cex=1.2,lwd=2)

  # bar.[,"2024"]/bar.[,"2000"]
  # colSums(sp_ba$Kaminokodaira)

}

# Fig_yr_ba_kaminokodaira_zone_2024 ####
#' Draw a figure relationships between year and basal area ratio for species main distribution zone in Kagamiishi in 2024
#'
#' @return nothing, only output figure
#' @export
#'
#' @examples
#' Fig_yr_ba_kaminokodaira_zone_2024()
Fig_yr_ba_kaminokodaira_zone_2024 <- function(){

  .<-sp_ba_ratio
  plot. <- "Kaminokodaira"
  bar.<-.[[plot.]]
  sp.<-rownames(bar.)
  zone.<-SpeciesList2$zone[match(sp.,SpeciesList2$spj)]
  rownames(bar.)<-zone.
  bar..<-aggregate(. ~ zone., data = data.frame(bar.), FUN = sum)

  Year <- as.numeric(colnames(bar.))
  rba.Temperate <- bar..[bar..$zone.=="Temperate",-1]/bar..[bar..$zone.=="Temperate",2]   #relative basal area
  rba.Ecotone   <- bar..[bar..$zone.==    "Ecotone",-1]/bar..[bar..$zone.=="Ecotone",2]   #relative basal area
  rba.Subarctic <- bar..[bar..$zone.=="Subarctic",-1]/bar..[bar..$zone.=="Subarctic",2]   #relative basal area


  plot(Year,rba.Subarctic,ylab="Ratio of total basal area", ylim=c(0.5,1.1),
       type="b",lwd=2,pch=17,col="skyblue", main="In the Ecotone Plot")
  lines(Year,rba.Ecotone,type="b",col="purple",lwd=2,pch=8)
  lines(Year,rba.Temperate,type="b",col="orange",lwd=2,pch=16)
  abline(h=1,col="red",lty=2,lwd=2)
  legend(2000,0.7,c("Subarctic tree species","Ecotone tree species","Temperate tree species"),lwd=2,pch=c(17,8,16),col=c("skyblue","purple","orange"),cex=1)

}



#' Draw simple pi chart
#'
#' @param X   vector of pichart data
#' @param r  radius of pichart circle
#' @param x  x coordinate of center point
#' @param y  y coordinate of center point
#'
#' @return no return values, only draw pichart
#' @export
#'
#' @examples
#' X <-c(1,5,6)
#' plot(1:3)
#' pichart(X,rx=0.1,ry=0.1,x=2.5,y=2.5,col=c("Orange","Purple","SkyBlue"))
#' pichart(X,rx=0.1,ry=0.1,x=1.5,y=1.5,col=c("Orange","Purple","SkyBlue"),density =c(NA,20,10))
#'
#'
#'
pichart <- function(X,rx=1,ry=1,x=0,y=0,col=1:length(X),density =NA){ #col=1:length(X)
  stp <- seq(0,2*pi,0.001)
  x. <- rx * sin(stp) + x
  y. <- ry * cos(stp) + y
  n<-length(stp)
  j<-c(1,round(n*cumsum(X)/sum(X)))

  lines(x.,y.,type="l")
  for(i in 1:length(X)){
    j.<-j[i]:j[i+1]
    polygon(c(x,x.[j.]),c(y,y.[j.]),col=col[i],density =density[i]) #
  }
}


#' Return a data frame of total basal area ratio for each plot
#'
#' grouping with main distribution zone "Temperate", "Ecotone", "Subarctic"
#'
#' @param term term of monitoring
#' @return a data frame
#' @export
#'
#' @examples
#' plt
#' sp_zone_ba_ratio_calc(1)
#' sp_zone_ba_ratio_calc(7)
#' (sp_zone_ba_ratio<-lapply(1:7,sp_zone_ba_ratio_calc))
#' # save(sp_zone_ba_ratio,file="data/sp_zone_ba_ratio.RData")
#'
#'
sp_zone_ba_ratio_calc<-function(term=1){
  ratio.<-matrix(nrow=nrow(plt),ncol=3,0,
                 dimnames = list(plt$na, c("Temperate", "Ecotone", "Subarctic")))

  for(ii in 1:nrow(plt)){
    d.<-sp_ba_ratio[[ii]]
    if(term>ncol(d.))next
    sp.<-rownames(d.)
    i<-match(sp.,SpeciesList$spj)
    z<-SpeciesList$zone[i]
    ratio.[ii,"Temperate"]<-sum(d.[z=="Temperate",term])
    ratio.[ii,"Ecotone"]<-sum(d.[z=="Ecotone",term])
    ratio.[ii,"Subarctic"]<-sum(d.[z=="Subarctic",term])
  }

  return(ratio.)
}



#' Return a data frame of total basal area ratio for each plot
#' (Living + Standing Dead)
#'
#' grouping with main distribution zone "Temperate", "Ecotone", "Subarctic"
#'
#' @param term term of monitoring
#' @return a data frame
#' @export
#'
#' @examples
#' plt
#' sp_zone_ba_ratio_calc(1)
#' sp_zone_ba_ratio_calc(7)
#' (sp_zone_ba_ratio<-lapply(1:7,sp_zone_ba_ratio_calc))
#' # save(sp_zone_ba_ratio,file="data/sp_zone_ba_ratio.RData")
#'
#'
sp_zone_ba_ratio_calc2<-function(term=1){
  ratio.<-matrix(nrow=nrow(plt),ncol=6,0,
                 dimnames = list(plt$na, c("Temperate_live","Temperate_StandingDead", "Ecotone_live", "Ecotone_StandingDead", "Subarctic_live","Subarctic_StandingDead")))

  sp_zone_ba_live_dead <-c()
  sp_zone_ba[[1]]+sp_zone_ba_dead[[1]]

  for(ii in 1:nrow(plt)){
    d.<-sp_ba_ratio[[ii]]
    if(term>ncol(d.))next
    sp.<-rownames(d.)
    i<-match(sp.,SpeciesList$spj)
    z<-SpeciesList$zone[i]
    ratio.[ii,"Temperate"]<-sum(d.[z=="Temperate",term])
    ratio.[ii,"Ecotone"]<-sum(d.[z=="Ecotone",term])
    ratio.[ii,"Subarctic"]<-sum(d.[z=="Subarctic",term])
  }

  return(ratio.)
}


#' Return a data frame of total basal area  for each plot
#'
#' grouping with main distribution zone "Temperate", "Ecotone", "Subarctic"
#'
#' @param ba_list list of data frame for each plot
#' @param term term of monitoring
#' @return a data frame
#' @export
#'
#' @examples
#' plt
#' sp_zone_ba_calc(ba_list=sp_ba,term=1)
#' sp_zone_ba<-c()
#' for (i in 1:7){
#'  sp_zone_ba<-c(sp_zone_ba,list(sp_zone_ba_calc(ba_list=sp_ba,term=i)))
#' }
#' sp_zone_ba
#' # save(sp_zone_ba,file="data/sp_zone_ba.RData")
#'
#' sp_zone_ba_calc(ba_list=sp_ba_StandigDead,term=1)
#' sp_zone_ba_dead<-c()
#' for (i in 1:7){
#'  sp_zone_ba_dead<-c(sp_zone_ba_dead,list(sp_zone_ba_calc(ba_list=sp_ba_StandigDead,term=i)))
#' }
#' sp_zone_ba_dead
#' # save(sp_zone_ba_dead,file="data/sp_zone_ba_dead.RData")
#'
sp_zone_ba_calc<-function(ba_list=sp_ba,term=1){
  table.<-matrix(nrow=nrow(plt),ncol=3,0,
                 dimnames = list(plt$na, c("Temperate", "Ecotone", "Subarctic")))

  for(ii in 1:nrow(plt)){
    d.<-ba_list[[ii]]
    if(term>ncol(d.))next
    sp.<-rownames(d.)
    i<-match(sp.,SpeciesList$spj)
    z<-SpeciesList$zone[i]
    table.[ii,"Temperate"]<-sum(d.[z=="Temperate",term])
    table.[ii,"Ecotone"]<-sum(d.[z=="Ecotone",term])
    table.[ii,"Subarctic"]<-sum(d.[z=="Subarctic",term])
  }

  return(table.)
}

#' Return a data frame of total basal area ratio for each plot
#'  grouping with dominant tree species "スギ","ブナ","オオシラビソ"
#'
#' @return a data frame of total basal area ratio
#' @export
#'
#' @examples
#' sp_dominant_ba_ratio_calc()
sp_dominant_ba_ratio_calc <- function(){
  plt <- TateyamaForest2024$plot_profile
  sp_dominant<-c("スギ","ブナ","オオシラビソ")
  ratio.<-c() # sp_dominant_ba_ratio
  for(ii in 1:nrow(plt)){
    d.<-sp_ba_ratio[[ii]]
    sp.<-rownames(d.)

    ratio.<-rbind(ratio.,d.[match(sp_dominant,sp.),1])
  }
  ratio.[is.na(ratio.)]<-0
  colnames(ratio.)<-sp_dominant
  rownames(ratio.)<-plt$na
  sp_dominant_ba_ratio<-ratio.
  return(sp_dominant_ba_ratio)
}

# Abies ####

#' histgram of dbh including dead standing trees
#' in 2000 (term 1) at Kaminokodaira
#'
#' @param plotname
#' @param species
#' @param term
#'
#' @return
#' @export
#'
#' @examples
#' par(mfrow=c(1,3))
#' dbh_hist_term1to7(plotname="Kaminokodaira",main="Ecotone plot",species="オオシラビソ",legend=F)
#' dbh_hist_term1to7(plotname="Matsuotoge",main="Subarctic plot",species="オオシラビソ",legend=F)
#' dbh_hist_term1to7(plotname="Kagamiishi",main="Timberline plot",species="オオシラビソ",legend=F,breaks=seq(0,30,5))
#' legend(1.5,53,legend=c("Dead Standing (fallen)","Dead Standing ","Living"),
#'  fill=c("black","black","white"),density=c(NA,20,NA))
#'
dbh_hist_term1to7 <- function(plotname="Kaminokodaira",main="",species="オオシラビソ",
                              dbh_min=10,breaks=seq(10,50,10),legend=TRUE,...){

  #TateyamaForest2024
  d <-subset(dd4,sp==species & plot==plotname)
  dbh. <- d$d01
  f1.   <- d$f01
  f7.   <- d$f07
  #　途中加入木を除外
  i1 <- !is.na(d$f01) & d$f01>0                 # 01期 生存
  i2 <- !is.na(d$f01) & d$f01==0 & d$f07==0　   # 01期 立ち枯れ　07立ち枯れ
  i3 <- !is.na(d$f01) & d$f01==0 & d$f07==-1　　# 01期 立ち枯れ　07期倒れ


  dbh..<-rbind(
    table(cut(dbh.[i1],breaks)),
    table(cut(dbh.[i2],breaks)),
    table(cut(dbh.[i3],breaks))
  )

  #par(mfrow=c(1,1))
  barplot(dbh..,names=rev(rev(breaks)[-1]),
          main=main,
          col=c("white","black","black"),density=c(NA,20,NA),
          xlab="DBH (cm)",ylab="Number of trees",
          cex.main=1.8,
          cex.axis=1.2,cex.names=1.2,cex.lab=1.4)
  if(legend){
    legend(0,10,legend=c("Dead Standing (fallen)","Dead Standing ","Living"),
           fill=c("black","black","white"),density=c(NA,20,NA))
  }


}

#' summarize field stand data  to table composed of species(sp), DBH,vitality index
#'
#' @param plot.name
#'
#' @return a data frame of plot species(sp), DBH(D),vitality index(f)
#' @export
#'
#' @examples
#' Df_table("Kagamiishi")
#' (d.<-Df_table("Arimine"))
#'
#'
Df_table<-function(plot.name="Arimine"){
  . <- TateyamaForest2024
  d0=.$d0; plt=.$plot_profile;cnD=.$colnames_D;cnf=.$colnames_f;yr=.$yr
  d.<-d0[[plot.name]]
  f.<-d.[, na.omit(as.character(cnf[plot.name,]))]
  D.<-d.[,na.omit(as.character(cnD[plot.name,]))]
  return(data.frame(sp=d.$sp,f.,D.))
}

# Snow ####
#' Fig_snow_cover
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
#' Fig_snow_cover()
#'
Fig_snow_cover<- function(dat=snow){

  op <- par(no.readonly=T)

  CP <- T  # カラーか白黒か

  grdf <- data.frame(pl = c("浄土山","鏡石","松尾峠","美松","上ノ小平",
                            "有峰","ブナ平","ブナ坂","美女平","富山"),
                     pch = c(24,21,22,24, 4,23,25,21, 8, 17),
                     bg = if(CP) c(5,NA,NA,NA,NA,NA,NA,2,NA,NA) else c(8,NA,NA,NA,NA,NA,NA,1,NA,NA),
                     lty = c( 3, 1, 3, 1, 1, 1, 3, 1, 3, 1),
                     col = if(CP) c(5,5,4,4,3,2,2,2,2,1) else 1,
                     stringsAsFactors=F
  )[c(2,3,5,8),]

  grdf$col=rev(c( "darkolivegreen4", "blueviolet" , "blue", "cyan3"  ))
  grdf$pch=rev(c(13,11,17,8))
  grdf$lty=c(2,2,2,2)


  gd <- dat
  gd$val <- gd$pe

  gd <- subset(gd,select=c(pl,yr,val))

  gd <- subset(gd,yr >= 1999)

  table(gd$pl,gd$yr)
  all(table(gd$pl,gd$yr) == 1)



 # par(mar=c(3.5,4.5,1,1))
  plot(gd$yr,gd$val,type="n",xaxt="n",
       xlim=range(gd$yr),ylim=c(0,max(gd$val,na.rm=T)),
       ann=F,bty="l",las=1)
  axis(1,min(gd$yr):max(gd$yr),las=2,cex=0.8)
  mtext("Annual snow cover duration (days)",2,3)
  mtext("Year",1,3)

  for(i in 1:nrow(grdf)){
    cpl <- grdf$pl[i]
    gds <- subset(gd,pl == cpl)
    gds <- gds[order(gds$yr),]
    cdf <- subset(grdf,pl == cpl)
    lines(gds$yr,gds$val,type="b",
          col=cdf$col,bg=cdf$bg,pch=cdf$pch,lty=cdf$lty)
  }

  n<-c(2,4,5,7)
  plot_name <-c("Temperate plot","Ecotone plot","Subarctic plot","Timberline plot")
  legend(2010,80,plot_name,pch=leg$pch[n],col=leg$col[n],lty=leg$lty[n],cex=0.8)

  par(op)
}



#' Fig_snow_depth
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
#' Fig_snow_depth()
#'
Fig_snow_depth<- function(dat=snow){

  op <- par(no.readonly=T)

  CP <- T  # カラーか白黒か

  grdf <- data.frame(pl = c("浄土山","鏡石","松尾峠","美松","上ノ小平",
                            "有峰","ブナ平","ブナ坂","美女平","富山"),
                     pch = c(24,21,22,24, 4,23,25,21, 8, 17),
                     bg = if(CP) c(5,NA,NA,NA,NA,NA,NA,2,NA,NA) else c(8,NA,NA,NA,NA,NA,NA,1,NA,NA),
                     lty = c( 3, 1, 3, 1, 1, 1, 3, 1, 3, 1),
                     col = if(CP) c(5,5,4,4,3,2,2,2,2,1) else 1,
                     stringsAsFactors=F
  )[c(3,5,8),]

  # grdf$col=c( "darkolivegreen4", "blueviolet" , "blue" )
  # grdf$pch=c(13,11,17)
  # grdf$lty=c(2,2,2)

  grdf$col=c( "blue", "blueviolet" , "darkolivegreen4" )
  grdf$pch=c(17,11,13)
  grdf$lty=c(2,2,2)



  gd <- dat
  gd$val <- gd$dep
  gd <- subset(gd,select=c(pl,yr,val))
  gd <- subset(gd,yr >= 2004)

  table(gd$pl,gd$yr)
  all(table(gd$pl,gd$yr) == 1)


 # par(mar=c(3.5,4.5,1,1))
  plot(gd$yr,gd$val,type="n",xaxt="n",
       xlim=range(gd$yr),ylim=c(0,max(gd$val,na.rm=T)),
       ann=F,bty="l",las=1)
  axis(1,min(gd$yr):max(gd$yr),las=2)
  mtext("Maximum snow depth (cm)",2,3)
  mtext("Year",1,3)

  for(i in 1:nrow(grdf)){
    cpl <- grdf$pl[i]
    gds <- subset(gd,pl == cpl)
    gds <- gds[order(gds$yr),]
    cdf <- subset(grdf,pl == cpl)
    lines(gds$yr,gds$val,type="b",
          col=cdf$col,bg=cdf$bg,pch=cdf$pch,lty=cdf$lty)
  }

  n<-c(2,4,5)
  plot_name <-c("Temperate plot","Ecotone plot","Subarctic plot")
  legend(2011,180,plot_name,pch=leg$pch[n],col=leg$col[n],lty=leg$lty[n],cex=0.8)

  #par(op)
}
