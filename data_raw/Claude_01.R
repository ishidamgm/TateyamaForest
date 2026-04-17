# Claude_01.R

library(package="TateyamaForest")
help(package="TateyamaForest")
# data(package="TateyamaForest")

# 各プロット・各調査期間のA. mariesii死亡個体数または死亡率 ####
plt2 # 調査区データ
dd4

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


# 各プロットの年別温量指数（黒部ダムから推定済みの値）####
kurobe_dam_temperature

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

# csv for Claude ############
# Claude_01_export.R
load("plt2.RData")
load("dd4.RData")
load("kurobe_dam_temperature.RData")


# 1. 黒部ダム気温データ
write.csv(kurobe_dam_temperature, "kurobe_dam_temperature.csv", row.names = FALSE)

# 2. プロット基本情報
write.csv(plt2, "plt2.csv", row.names = FALSE)

# 3. 個体データ（オオシラビソのみ抽出）
abies <- subset(dd4, sp == "オオシラビソ")
write.csv(abies, "dd4_abies.csv", row.names = FALSE)

# WI_with_KurodeDamObservation
write.csv(WI_with_KurodeDamObservation, "WI_with_KurodeDamObservation.csv", row.names = FALSE)

# 4. dd4の列名確認
cat("dd4 columns:\n")
print(names(dd4))
cat("\ndd4 head:\n")
print(head(dd4, 3))

# body of death_ratio ####
death_ratio

# 確認事項： 20260323 ####

## 密度の数値（特にTimberline: 3,008 stems/ha）の確認 ####
plt5$na
i<-1
d<-subset(dd5,plot==plt5$na[i])
nrow(d)
apply(d[,clm_dbh],1,max,na.rm=T)<10
apply(d[,clm_dbh],1,pmax,na.rm=T)
clm_dbh
max_dbh <- do.call(pmax, c(d[, clm_dbh], na.rm = TRUE))
sum(!is.na(max_dbh))

d[610,]

# 20260326 ####

## Fig. 12の実際の再設計作業 ####

## Section 2.5の統計解析記述の最終確認 ####

# データ書き出し用コード

# 1. BAデータ（各プロット・各期間・樹種別）
# ba_dataというオブジェクト名は実際のものに合わせてください
write.csv(ba_data, "ba_data.csv", row.names = FALSE)

# 2. 積雪深・積雪期間の年別データ
snow
# write.csv(snow, "snow_data.csv", row.names = FALSE)
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

# 3. Fig.13用累積死亡率データ
# すでにdd4_abies.csvとplt2.csvから計算可能ですが、
# 既存のオブジェクトがあればそのまま書き出してください
write.csv(cumulative_mortality, "cumulative_mortality.csv", row.names = FALSE)

# 4. dd4全種版（Fig.11のDBH分布用）
write.csv(dd4, "dd4_all_species.csv", row.names = FALSE)


# fig7b_species_ratio.csv　####

#load("sp_ba_ratio.RData")
#load("SpeciesList2.RData")  # またはパッケージからロード

plot. <- "Kaminokodaira"
bar.  <- sp_ba_ratio[[plot.]]
sp.   <- rownames(bar.)
Year  <- as.numeric(colnames(bar.))

# Panel (a): zone別相対BA
zone. <- SpeciesList2$zone[match(sp., SpeciesList2$spj)]
bar_zone <- bar.
rownames(bar_zone) <- zone.
bar_agg <- aggregate(. ~ zone.,
                     data = data.frame(bar_zone), FUN = sum)

result_a <- data.frame(Year = Year)
for (z in c("Temperate","Ecotone","Subarctic")) {
  vals <- as.numeric(bar_agg[bar_agg$zone. == z, -1])
  result_a[[z]] <- vals / vals[1]
}
write.csv(result_a, "fig7a_zone_ratio.csv", row.names = FALSE)

# Panel (b): 主要樹種相対BA（%）
result_b <- data.frame(Year = Year)
for (sp_j in c("オオシラビソ","ブナ","スギ")) {
  if (sp_j %in% rownames(bar.)) {
    vals <- as.numeric(bar.[sp_j, ])
    result_b[[sp_j]] <- 100 * vals / vals[1]
  }
}
write.csv(result_b, "fig7b_species_ratio.csv", row.names = FALSE)
cat("Done\n")
