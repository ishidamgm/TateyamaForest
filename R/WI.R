# WI.R

degree <- pi / 180


#' Title
#'
#' @param t12
#'
#' @return
#' @export
#'
#' @examples
#' wi_calc()
#' (t. <- plt4[,paste0("t",1:12)])
#'wi_calc(t.[1,])
#'apply(t.,1,wi_calc)
#'
wi_calc <- function(T12=1:12){
  T12<-as.numeric(T12)
  wi. <- T12-5
  wi.[wi.<0]<-0
  return(sum(wi.))
}

#' Title
#'
#' @param t12
#'
#' @return
#' @export
#'
#' @examples
#' ci_calc()
#' (t. <- plt5[,paste0("t",1:12)])
#'ci_calc(t.[1,])
#'apply(t.,1,wi_calc)
#'apply(t.,1,ci_calc)
ci_calc <- function(T12=1:12){
  T12<-as.numeric(T12)
  ci. <- T12-5
  ci.[ci.>0]<-0
  return(sum(ci.))
}



#' Return mean temperature from year with
#'  regression function between year and mean temperature in Kurobe dam
#'
#'
#' @param year
#'
#' @return mean temperature
#' @export
#'
#' @examples
#' kurobe_dam_temperature
#' kurobe_dam_temperature_regression
#' year<-1960:2023
#' plot(year,F_kurobe_dum_mean_tmp(year))
#' year00<-1953+14
#' abline(v=year00,col="red")  # base year of mesh data (1953-1982)
#'
F_kurobe_dum_mean_tmp <-function(year) {
  -47.932410 +   0.027340 *year
}

#' Title
#'
#' @return
#'
#' @export
#'
#'
#'
#'
#' @examples
#' Fig_kurobe_dam_temperature()
#'
#'
Fig_kurobe_dam_temperature <- function(){

  d<-kurobe_dam_temperature
  # plot ####
  plot(d$year,d$max,type="b",pch=24,col="red",ylim=c(0,14),xlab="Year",ylab="Temperature (°C)",cex.lab=1.1)
  lines(d$year,d$min,type="b",pch=25,col="blue")
  lines(d$year,d$mean,pch=16,type="b")
  # regression ####
  lm_max<-lm(d$max~d$year)
  lm_min<-lm(d$min~d$year)
  lm_mean<-lm(d$mean~d$year)
  lm_<-list(summary(lm_max),summary(lm_min),summary(lm_mean))


  # regression lines ####
  abline(lm(d$max~d$year),col="red")
  abline(lm(d$min~d$year),col="blue")
  abline(lm(d$mean~d$year))

  legend(1968,14,c("Maximum","Mean","Minimum"),pch=c(24,16,25),col=c("red","black","blue"),cex=0.9)

}


## alt-WI ####
#' Fig_alt_WI
#'
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI()
#' ba.<-sp_zone_ba_ratio[[1]]
#' plt<-TateyamaForest2024$plot_profile
#' for(ii in 1:nrow(plt)){
#'   pichart(ba.[ii,],rx=40,ry=3,x=plt$alt[ii],y=plt$WI[ii],col=c("Orange","Purple","SkyBlue"))
#' }
#' text(plt$alt,plt$WI,plt$na,cex=0.8)
#'
Fig_alt_WI<-function(){
  plt<-TateyamaForest2024$plot_profile
  plot(plt$alt,plt$WI,type="n",xlab="Alitude (m)", ylab="Warmth Index (Degrees Celsius*Month)",
       xlim=c(900,2300),ylim=c(25,70))
  text(plt$alt,plt$WI,plt$na,cex=0.8)
  abline(h=c(45,55),lty=2)

  text(1950,60,"Temperate zone",cex=3,col="Orange")
  text(1400,35,"Subarctic zone",cex=3,col="SkyBlue")
  text(1800,50,"Ecotone",cex=3,col="Purple")

}

#' Fig_alt_WI2
#'
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI2(plt3)
#'
Fig_alt_WI2<-function(plt2,term=1){
  plot(plt2$alt,plt2$WI,type="n",xlab="Alitude (m)", ylab="Warmth Index (Degrees Celsius*Month)",
       xlim=c(1000,2300),ylim=c(22,68))
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_ratio[[term]]
  ba.<-ba.[match(plt2$na,rownames(ba.)),]
  for(ii in 1:nrow(plt2)){
    pichart(ba.[ii,],rx=40,ry=3,x=plt2$alt[ii],y=plt2$WI[ii],col=c("Orange","Purple","SkyBlue"))
  }
  # labels of picharts
  text(plt2$alt,plt2$WI-5,plt2$na,cex=0.8)

  # zone label
  text(1950,60,"Temperate zone",cex=2,col="Orange")
  text(1400,35,"Subarctic zone",cex=2,col="SkyBlue")
  text(1800,50,"Ecotone",cex=2,col="Purple")

  legend(1600,32,c("Temperate species","Subarctic species","Ecotone species"),col=c("Orange","Purple","SkyBlue"),pch=15)

}



#' Fig_alt_WI3
#'
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' plt2<-TateyamaForest2024$plot_profile[-c(1,3,6,8),]
#' Fig_alt_WI2(plt2)
#' Fig_alt_WI3(plt3)
#'
Fig_alt_WI3<-function(plt2,term=1){
  plot(plt2$alt,plt2$WI,type="n",xlab="Alitude (m)", ylab="Warmth Index (Degrees Celsius*Month)",
       xlim=c(1000,2300),ylim=c(22,68))
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_ratio[[term]]
  ba.<-ba.[match(plt2$na,rownames(ba.)),]
  for(ii in 1:nrow(plt2)){
    pichart(ba.[ii,],rx=40,ry=3,x=plt2$alt[ii],y=plt2$WI[ii],col=c("Orange","Purple","SkyBlue"))
  }
  # labels of picharts
  text(plt2$alt,plt2$WI-5,plt2$na,cex=0.8)

  # zone label
  text(1950,60,"Temperate zone",cex=3,col="Orange")
  text(1400,35,"Subarctic zone",cex=3,col="SkyBlue")
  text(1800,50,"Ecotone",cex=3,col="Purple")

  legend(1600,30,c("Temperate species","Subarctic species","Ecotone species"),col=c("Orange","Purple","SkyBlue"),pch=15)

}



#' Fig_alt_WI4
#'　including standing dead trees
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI3(plt2,term=1)
#' Fig_alt_WI4(plt2,term=1)
#'
#'
Fig_alt_WI4<-function(plt2,term=1){
  plot(plt2$alt,plt2$WI,type="n",xlab="Alitude (m)", ylab="Warmth Index (Degrees Celsius*Month)",
       xlim=c(1000,2300),ylim=c(22,68))
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_live_dead2_ratio[[term]]
  ba.<-ba.[match(plt2$na,rownames(ba.)),]
  for(ii in 1:nrow(plt2)){#ii=1
    pichart(as.numeric(ba.[ii,]),rx=40,ry=3,x=plt2$alt[ii],y=plt2$WI[ii],
            col=c("Orange","Orange","Purple","Purple","SkyBlue","SkyBlue"),
            density=c(NA,20,NA,20,NA,20)
            )


  }
  # labels of picharts
  text(plt2$alt,plt2$WI-5,plt2$na,cex=0.8)

  # zone label
  text(1950,60,"Temperate zone",cex=3,col="Orange")
  text(1400,35,"Subarctic zone",cex=3,col="SkyBlue")
  text(1800,50,"Ecotone",cex=3,col="Purple")

}


#' Fig_alt_WI4
#'　including standing dead trees
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI3(plt2,term=1)
#' Fig_alt_WI4(plt2,term=1)
#'　 Fig_alt_WI4_zone(plt2,term=1)
#'
Fig_alt_WI4_zone<-function(plt2,term=1){
  plot(plt2$alt,plt2$WI,type="n",xlab="Above Sea Level (m)", ylab="Warmth Index (Degrees Celsius*Month)",
       xlim=c(1000,2300),ylim=c(22,68))
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_live_dead2_ratio[[term]]
  ba.<-ba.[match(plt2$na,rownames(ba.)),]
  for(ii in 1:nrow(plt2)){#ii=1
    pichart(as.numeric(ba.[ii,]),rx=40,ry=3,x=plt2$alt[ii],y=plt2$WI[ii],
            col=c("Orange","Orange","Purple","Purple","SkyBlue","SkyBlue"),
            density=c(NA,20,NA,20,NA,20)
    )


  }
  # labels of picharts
  plot_name <-c("Temperate plot","Ecotone plot","Subarctic plot","Timberline plot")
  #text(plt2$alt,plt2$WI-5,plt2$na,cex=0.8)
  text(plt2$alt,plt2$WI-5,  plot_name,cex=0.8)
  # zone label
  text(1950,60,"Temperate zone",cex=3,col="Orange")
  text(1400,35,"Subarctic zone",cex=3,col="SkyBlue")
  text(1800,50,"Ecotone",cex=3,col="Purple")

}

#' Fig_alt_WI4_2020_JP
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI4_2020_JP()
#'
#'
Fig_alt_WI4_2020_JP<-function(plt=plt5,term=1){
  plot(plt$alt,plt$WI,type="n",xlab="標高 (m)", ylab="温量指数 (℃・月)",
       xlim=c(1000,2300),ylim=c(22,68))
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_live_dead2_ratio[[term]]
  ba.<-ba.[match(plt$na,rownames(ba.)),]
  ba.<-    ba.[,seq(1,6,2)]
  for(ii in 1:nrow(plt)){#ii=1
    pichart(as.numeric(ba.[ii,]),rx=40,ry=3,x=plt$alt[ii],y=plt$WI[ii],
            col=c("Orange","Purple","SkyBlue")
    )

  }
  # labels of picharts
  text(plt$alt,plt$WI-4,plt2$na,cex=0.8)

  # zone label
  text(1950,60,"温帯",cex=2,col="Orange")
  text(1800,50,"移行帯",cex=2,col="Purple")
  text(1600,40,"亜寒帯",cex=2,col="SkyBlue")

  legend(1025,37,
         legend=c("温帯樹種","移行帯樹種","移行帯樹種"),
         fill=c("Orange","Purple","SkyBlue"),
         title="胸高断面積合計割合",
         title.cex=1.1)

}

#' Fig_alt_WI4_2020_JP
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI4_2020_JP()
#'
#'
Fig_alt_WI4_2020_JP<-function(plt=plt5,term=1){
  plot(plt$alt,plt$WI,type="n",xlab="標高 (m)", ylab="温量指数 (℃・月)",
       xlim=c(1000,2300),ylim=c(22,68))
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_live_dead2_ratio[[term]]
  ba.<-ba.[match(plt$na,rownames(ba.)),]
  ba.<-    ba.[,seq(1,6,2)]
  for(ii in 1:nrow(plt)){#ii=1
    pichart(as.numeric(ba.[ii,]),rx=40,ry=3,x=plt$alt[ii],y=plt$WI[ii],
            col=c("Orange","Purple","SkyBlue")
    )

  }
  # labels of picharts
  text(plt$alt,plt$WI-4,plt2$na,cex=0.8)

  # zone label
  text(1950,60,"温帯",cex=2,col="Orange")
  text(1800,50,"移行帯",cex=2,col="Purple")
  text(1600,40,"亜寒帯",cex=2,col="SkyBlue")

  legend(1025,37,
         legend=c("温帯樹種","移行帯樹種","移行帯樹種"),
         fill=c("Orange","Purple","SkyBlue"),
         title="胸高断面積合計割合",
         title.cex=1.1)

}
#' Fig_alt_WI4_2020_JP
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI4_2020()
#'
#'
Fig_alt_WI4_2020<-function(plt=plt5,term=1){
  plot(plt$alt,plt$WI,type="n",xlab="Altitude (m)", ylab="Warmth Index (degrees Celsius*month)",
       xlim=c(1000,2300),ylim=c(22,68),cex.lab=1.2)
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_live_dead2_ratio[[term]]
  ba.<-ba.[match(plt$na,rownames(ba.)),]
  ba.<-    ba.[,seq(1,6,2)]
  for(ii in 1:nrow(plt)){#ii=1
    pichart(as.numeric(ba.[ii,]),rx=40,ry=3,x=plt$alt[ii],y=plt$WI[ii],
            col=c("Orange","Purple","SkyBlue")
    )

  }
  # labels of picharts
  text(plt$alt,plt$WI-4,plt2$na,cex=0.8)

  # zone label
  text(1950,60,"Temperate zone",cex=2,col="Orange")
  text(1800,50,"Ecotone",cex=2,col="Purple")
  text(1600,40,"Subarctic zone",cex=2,col="SkyBlue")


  legend(1025,37,
         legend=c("Temperate tree species","Ecotone tree species","Subarctic tree species"),
         fill=c("Orange","Purple","SkyBlue"),
         title="Ratio of basal area",
         title.cex=1.0)

}

#' Fig_alt_WI4_2020_JP
#' FEM:Fig.7
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI4_2020_zone()
#'
#'
Fig_alt_WI4_2020_zone<-function(plt=plt5,term=1){
  plot(plt$alt,plt$WI,type="n",xlab="Above Sea Level (m)", ylab="Warmth Index (°C*month)",
       xlim=c(1000,2350),ylim=c(20,68),cex.lab=1.2)
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_live_dead2_ratio[[term]]
  ba.<-ba.[match(plt$na,rownames(ba.)),]
  ba.<-    ba.[,seq(1,6,2)]
  for(ii in 1:nrow(plt)){#ii=1
    pichart(as.numeric(ba.[ii,]),rx=40,ry=3,x=plt4$alt[ii],y=plt4$WI[ii],
            col=c("Orange","Purple","SkyBlue")
    )

  }
  # labels of picharts
   plot_name <-c("Temperate plot","Ecotone plot","Subarctic plot","Timberline plot")
  #text(plt2$alt,plt2$WI-5,plt2$na,cex=0.8)
  text(plt4$alt,plt4$WI-5,  plot_name,cex=0.8)

  # zone label
  text(1950,60,"Temperate zone",cex=2,col="Orange")
  text(1800,50,"Ecotone",cex=2,col="Purple")
  text(1600,40,"Subarctic zone",cex=2,col="SkyBlue")


  legend(1025,37,
         legend=c("Temperate tree species","Ecotone tree species","Subarctic tree species"),
         fill=c("Orange","Purple","SkyBlue"),
         title="Ratio of basal area",
         title.cex=1.0)

}


#' Fig_alt_WI4_2020_Live_Dead
#'　including standing dead trees
#'
#' @param term term of monitoring
#' @return not return anything, draw only.
#' @export
#'
#' @examples
#' Fig_alt_WI4_2020_Live_Dead()
#'
#'
Fig_alt_WI4_2020_Live_Dead<-function(plt=plt4,term=1){
  plot(plt$alt,plt$WI,type="n",xlab="標高 (m)", ylab="温量指数 (℃・月)",
       xlim=c(1000,2300),ylim=c(22,68))
  #text(plt2$alt,plt2$WI,plt2$na,cex=0.8)
  abline(h=c(45,55),lty=2)
  # pichart
  ba.<-sp_zone_ba_live_dead2_ratio[[term]]
  ba.<-ba.[match(plt$na,rownames(ba.)),]
  for(ii in 1:nrow(plt)){#ii=1
    pichart(as.numeric(ba.[ii,]),rx=40,ry=3,x=plt$alt[ii],y=plt$WI[ii],
            col=c("Orange","Orange","Purple","Purple","SkyBlue","SkyBlue"),
            density=c(NA,20,NA,20,NA,20)
    )


  }
  # labels of picharts
  text(plt2$alt,plt2$WI-5,plt2$na,cex=0.8)

  # zone label
  text(1950,60,"温帯",cex=3,col="Orange")
  text(1800,50,"移行帯",cex=3,col="Purple")
  text(1600,40,"亜寒帯",cex=3,col="SkyBlue")

  legend(1050,42,
         legend=c("温帯樹種・生存","温帯樹種・立枯","移行帯樹種・生存","移行帯樹種・立枯","亜寒帯樹種・生存","亜寒帯樹種・立枯"),
         fill=c("Orange","Orange","Purple","Purple","SkyBlue","SkyBlue"),
         text.col==c("Orange","Orange","Purple","Purple","SkyBlue","SkyBlue"),
         density=c(NA,20,NA,20,NA,20),
         title="胸高断面積合計割合")

}


#
#' Fig_year_WI
#'
#' @return
#' @export
#'
#' @examples
#'   wi_year
#' Fig_year_WI()
#'
#'
#'
Fig_year_WI<-function(){
  intact_plot <- c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")
  plot_no <- match(intact_plot,colnames(wi_year))
  legend_no <- match(intact_plot,leg$n)

  plot(0,type="n",xlab="Year", ylab="Warmth Index (Degrees Celsius*Month)",
       xlim=c(1960,2025),ylim=c(18,70),cex.lab=1.3,cex.axis=1.3)

  text(1972,68,"Temperate zone",cex=1.8,col="Orange")
  text(1970,50,"Ecotone",cex=1.8,col="Purple")
  text(1975,29,"Subarctic zone",cex=1.8,col="SkyBlue")


  abline(h=c(45,55),lty=2,lwd=3,col="red")
  for (i in 1:length(plot_no)){
    lines(rownames(wi_year),wi_year[,plot_no[i]],type="l",
          col=leg$col[legend_no[i]])

  }

  for (ii in match(intact_plot,plt4$na)){
    yr.<-plt4[ii,paste0("yr",1:7)]
    wi.<-plt4[ii,paste0("wi",1:7)]
    points(yr.,wi.)
    text(yr.-0,wi.+2,1:7)
  }

  text(1990,c(62,51,37,25),intact_plot,srt=7,cex=1.2)
  #legend(1960,80,intact_plot,lty=1,col=leg$col[legend_no],pch=leg$pch[legend_no])
}


#
#' Fig_year_WI2
#'
#' @return
#' @export
#'
#' @examples
#'   wi_year
#' Fig_year_WI2()
#'
Fig_year_WI2<-function(){
  intact_plot <- c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")
  plot_no <- match(intact_plot,colnames(wi_year))
  legend_no <- match(intact_plot,leg$n)

  plot(0,type="n",xlab="Year", ylab="Warmth Index (Degrees Celsius*Month)",
       xlim=c(1960,2025),ylim=c(22,80))

  text(1975,61,"Temperate zone",cex=2,col="Orange")
  text(1975,49,"Ecotone",cex=2,col="Purple")
  text(1975,36,"Subarctic zone",cex=2,col="SkyBlue")


  abline(h=c(45,55),lty=2,lwd=3,col="red")
  for (i in 1:length(plot_no)){
    lines(rownames(wi_year),wi_year[,plot_no[i]],type="b",
          col=leg$col[legend_no[i]],
          pch=leg$pch[legend_no[i]],
          cex=0.8)



  }

  legend(2000,34,intact_plot,lty=1,col=leg$col[legend_no],pch=leg$pch[legend_no])
}


#
#' Fig_year_WI_JP
#'
#' @return
#' @export
#'
#' @examples
#'   wi_year
#' Fig_year_WI_JP()
#'
#'
Fig_year_WI_JP<-function(){
  intact_plot <- c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")
  plot_no <- match(intact_plot,colnames(wi_year))
  legend_no <- match(intact_plot,leg$n)
  par(mgp=c(2.5, 1, 0))
  plot(0,type="n",xlab="西暦", ylab="温量指数 (月・℃)",
       xlim=c(1965,2025),ylim=c(18,70),cex.lab=1.3,cex.axis=1.3)

  text(1972,68,"温帯",cex=1.8,col="Orange")
  text(1970,50,"移行帯",cex=1.8,col="Purple")
  text(1975,29,"亜寒帯",cex=1.8,col="SkyBlue")


  abline(h=c(45,55),lty=2,lwd=3,col="red")
  for (i in 1:length(plot_no)){
    lines(rownames(wi_year),wi_year[,plot_no[i]],type="l",
          col=leg$col[legend_no[i]])

  }

  for (ii in match(intact_plot,plt4$na)){
    yr.<-plt4[ii,paste0("yr",1:7)]
    wi.<-plt4[ii,paste0("wi",1:7)]
    points(yr.,wi.)
    text(yr.-0,wi.+2,1:7)
  }
  text(1990,c(62,51,37,25),intact_plot,srt=7,cex=1.2)
}




#
#' Fig_year_WI_JP_2
#'
#' @return
#' @export
#'
#' @examples
#'   wi_year
#' Fig_year_WI_JP()
#'  Fig_year_WI_JP2()
#'
Fig_year_WI_JP2<-function(){
  intact_plot <- c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")
  plot_no <- match(intact_plot,colnames(wi_year))
  legend_no <- match(intact_plot,leg$n)
  par(mgp=c(2.5, 1, 0))
  plot(0,type="n",xlab="西暦", ylab="温量指数 (月・℃)",
       xlim=c(1965,2025),ylim=c(15,80),cex.lab=1.3,cex.axis=1.3)

  text(2010,75,"温帯",cex=1.8,col="Orange")
  text(2015,48,"移行帯",cex=1.8,col="Purple")
  text(2020,20,"亜寒帯",cex=1.8,col="SkyBlue")


  abline(h=c(45,55),lty=2,lwd=3,col="red")
  wi.<-WI_with_KurodeDamObservation
  for (i in 1:length(plot_no)){
    lines(rownames(wi.),wi.[,plot_no[i]],type="l",lty=i, lwd=2,#pch=i,
          col=leg$col[legend_no[i]])

  }

  for (ii in match(intact_plot,plt4$na)){
    yr.<-plt5[ii,paste0("yr",1:7)]
    wi.<-plt5[ii,paste0("wi",1:7)]
    points(yr.,wi.)
    text(yr.-0,wi.+2,1:7,cex=1)
  }


  legend(1965,82,plt5$na,cex=0.89,lty=1:4,lwd=2,col=leg$col[c(2,4,5,7)]) #pch=1:4,
}

#
#' Fig_year_WI_JP_2
#'
#' @return
#' @export
#'
#' @examples
#'   wi_year
#' Fig_year_WI_JP()
#'  Fig_year_WI_2()
#'
Fig_year_WI_2<-function(){
  intact_plot <- c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")
  plot_no <- match(intact_plot,colnames(wi_year))
  legend_no <- match(intact_plot,leg$n)
  par(mgp=c(2.5, 1, 0))
  plot(0,type="n",xlab="Year", ylab="Warmth Index (degrees Celsius*month)",
       xlim=c(1960,2025),ylim=c(15,80),cex.lab=1.1,cex.axis=1.1)

  text(1992,78,"Temperate zone",cex=1.4,col="Orange")
  text(2015,49,"Ecotone",cex=1.4,col="Purple")
  text(2010,20,"Subarctic zone",cex=1.4,col="SkyBlue")


  abline(h=c(45,55),lty=2,lwd=3,col="red")
  wi.<-WI_with_KurodeDamObservation
  for (i in 1:length(plot_no)){
    lines(rownames(wi.),wi.[,plot_no[i]],type="l",lty=i, lwd=2,#pch=i,
          col=leg$col[legend_no[i]])

  }

  # yr - wi cor.test ####-

#  yr= as.numeric(row.names(wi.))
# for ( ii in 1:4){
#   print(colnames(wi.)[ii])
#   print(summary(lm( wi.[,ii]~yr)))
# }


  #---------------
  for (ii in match(intact_plot,plt4$na)){
    yr.<-plt5[ii,paste0("yr",1:7)]
    wi.<-plt5[ii,paste0("wi",1:7)]
    points(yr.,wi.)
    text(yr.-0,wi.+2,1:7,cex=1)
  }


  #legend(1965,82,plt5$na,cex=0.89,lty=1:4,lwd=2,col=leg$col[c(2,4,5,7)]) #pch=1:4,
  legend(1960,82,plot_name,cex=0.89,lty=1:4,lwd=2,col=leg$col[c(2,4,5,7)]) #pch=1:4,

}


# '----------------------------------------' ####

#'  dd.mm.ss -> ddd.dddd
#'
#' @param d
#'
#' @return numeric degree
#'
#' @export
#'
#' @examples
#' s2n("36.300000")
#' s2n("36.5959900")
#'
s2n<-function(d){
  i=nchar(unlist(strsplit(d,"\\."))[1])+1;
  as.numeric(substr(d,1,i-1))+as.numeric(substr(d,i+1,i+2))/60+
    (as.numeric(substr(d,i+3,i+4))+as.numeric(paste(".",substr(d,i+5,20),sep="")))/3600
}


#' ddd.dddd -> dd.mm.ss
#'
#' @param d
#'
#' @return string degree, minutes,secant ddd.mmssss
#'
#' @export
#'
#' @examples
#' d=137.50
#' n2s(d)
#' s2n(n2s(d))
#' #  第7系原点 radian ########
#' (lat0 = s2n("36.00000")*degree)
#' (lon0 = s2n("137.100000")*degree)

n2s<-function(d)
{s1 = floor(d);
d2 = 60*(d - floor(d));
s2 = substring(1000 + floor(d2), 3, 4);
d3=60*(d2-floor(d2));
s3 = substring(10000 + 10*d3, 3, 5);
paste(s1,".", s2 ,s3,sep="")}

#' Tranlate to latitude and longitude  from mesh3 code
#'
#' @param M3Code
#'
#' @return latitude and longitude
#' @export
#'
#'
#' @examples
#'
#' M3CodeToLatLon(54377467)
#' n2s(M3CodeToLatLon(54377467))
#'
#'
#'
M3CodeToLatLon<-function(M3Code=54374091){
  #3次メッシュコードから緯度を得る
  lat1 =as.numeric(substr(M3Code, 1, 2)) #Val(Mid$(M3Code, 1, 2))
  lat2 = as.numeric(substr(M3Code, 5, 5))#VVal(Mid$(M3Code, 5, 1))
  lat3 = as.numeric(substr(M3Code, 7, 7))#VVal(Mid$(M3Code, 7, 1))
  M3CodeToLat = 2 / 3 * (lat1 + lat2 / 8 + lat3 / 80)
  lon1 =as.numeric(substr(M3Code, 3, 4)) #Val(Mid$(M3Code, 1, 2))
  lon2 = as.numeric(substr(M3Code, 6, 6))#VVal(Mid$(M3Code, 5, 1))
  lon3 = as.numeric(substr(M3Code, 8, 8))#VVal(Mid$(M3Code, 7, 1))
  M3CodeToLon =  100 + lon1 + lon2 / 8 + lon3 / 80
  return(cbind(M3CodeToLat, M3CodeToLon))
}


#' Tranlate mesh3 code from latitude nad longitude
#'
#' @param lat
#' @param lon
#'
#' @return mesh3 code
#'
#' @export
#'
#' @examples
#' sec <- 1/(360*60*60)
#'
#' code. <- "54376430" #54377467"
#'  (ll<-M3CodeToLatLon(code.))  #メッシュの左下の座標
#' (code <- LLToM3Code(lat=ll[1], lon=ll[2]))
#' code == code.
#'
#' #' メッシュ基点はメッシュの左下角
#' (code <- LLToM3Code(lat=ll[1]+10*sec, lon=ll[2]+10*sec))
#' code == code.
#' #' メッシュ基点からわずかに左下にずれた点は隣のメッシュ
#' (code <- LLToM3Code(lat=ll[1]-5*sec, lon=ll[2]-5*sec))
#' code == code.
#'
#'
#' plot(ll,xlim=c(ll[1]-60*sec,ll[1]+60*sec),ylim=c(ll[2]-60*sec,ll[2]+60*sec))
#' rect(ll[1],ll[2],ll[1]+45*sec,ll[2]+30*sec)
#' text(ll[1]+10*sec, ll[2]+10*sec,"in",col="blue")
#' text(ll[1]-5*sec, ll[2]-5*sec,"out !!",col="red")
#'
#'
#' # width and Height of mesh3 (45" 30" )
#'
#' lat. <- 36
#' earth_radius <- 40000/(2*pi)
#' lat.length <- cos(36/180*pi)*earth_radius*2*pi
#' heigth.m3<-30*lat.length*sec
#'widthth.m3<-45*40000*sec
#' data.frame(heigth.m3,widthth.m3,arae.m3=heigth.m3 * widthth.m3)
#'
#'
#'LLToM3Code(plt$ido,plt$kei)
#'
#'
LLToM3Code <- function(lat=36.63333, lon=137.58750) {
  #緯度、経度から3次メッシュコードを得る
  e = 0.00000000001

  LLToM3Code<-c()
  for (i in 1:length(lat)){
    #atitude
    l<-c()
    l[1] = trunc(lat[i] / (2 / 3) + e)
    l[3] = trunc((lat[i] - 2 / 3 * l[1]) / (2 / 3 * 1 / 8) + e)
    l[5] = trunc((lat[i] - 2 / 3 * l[1] - 2 / 3 * 1 / 8 * l[3]) / (2 / 3 * 1 / 80) + e)
    #longitude
    l[2] = trunc(lon[i])
    l[4] = trunc((lon[i] - l[2]) / (1 / 8) + e)
    l[6] = trunc((lon[i] - l[2] - l[4] * 1 / 8) / (1 / 80) + e)
    l[2] = l[2] - 100
    LLToM3Code =c(LLToM3Code,paste0(l,collapse=""))

  }

  return(LLToM3Code)
}






