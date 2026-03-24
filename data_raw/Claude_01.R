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
plt4$na
i<-1
d<-subset(dd5,plot==plt4$na[i])
apply(d[,clm_dbh],1,max,na.rm=T)<10
apply(d[,clm_dbh],1,pmax,na.rm=T)
clm_dbh
max_dbh <- do.call(pmax, c(d[, clm_dbh], na.rm = TRUE))

d[610,]
## 2024年WIデータが未入手である旨の脚注確認 ####

## Fig. 12の実際の再設計作業（データがあれば作図もお手伝いできます） ####

## Section 2.5の統計解析記述の最終確認 ####


