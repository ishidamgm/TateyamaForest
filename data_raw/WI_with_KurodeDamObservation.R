# WI_with_KurodeDamObservation.R
#　~/8T/Dropbox/00D/00/tateyama/TateyamaForest/works/kurobe/黒部ダム月別気温.R
# load("kurobe.RData")   #d(2010-2023), d2(1965-2001), d3(kurobe_dam_temperature)
# save.image(file = "kurobe.RData")

library(lubridate)

#　スプレッドシートのコピペ　clipr　####
library(clipr)
#　d..<-read.table(text = read_clip(), header = TRUE, sep = "\t")
# タブ区切りでクリップボードにコピー
#　write_clip(df, sep = "\t")



# 1965-2023　各年　2024年貸与されたデータ　　####
d3<-kurobe_dam_temperature
d3$date <- ISOdate(d3$year,6,15)
d3_plot<-function(){
  plot(d3$date,d3$min,ylim=c(-0,15),type="b")
  points(d3$date,d3$mean,col="black",type="b")
  points(d3$date,d3$max,col="red",type="b")
}

d3_plot()




# 2010-2023　各月　####
# 久米さん　黒部ダム2023関西電力.xlsx　####
names(d)<-c("year","month","tmax","tmin","mean")
d$date<-ISOdate(d$year,d$month,day=15)
plot((d$tmin+d$tmax)/2,d$mean)
d$mean<-(d$tmin+d$tmax)/2
d_plot<-function(){
 plot(d$date,d$tmax,type="l",col="red",ylim=c(-12,28))
 lines(d$date,d$tmin,col="blue")
 #lines(d$date,d$mean,col="black")
}

d_plot()

# d3 d2 確認　#####
d_year <- data.frame(year=unique(d$year),tmax=tapply(d$tmax,d$year,mean),tmin=tapply(d$tmin,d$year,mean),mean=tapply(d$mean,d$year,mean))
points(ISOdate(d_year$year,6,12),d_year$mean,pch=8,col="black")
points(ISOdate(d_year$year,6,12),d_year$tmin,pch=8,col="blue")
points(ISOdate(d_year$year,6,12),d_year$tmax,pch=8,col="red")

plot(d$tmax,d$tmin)
d_lm<-lm(tmin~tmax,data=d)
tmin_ <- as.numeric(predict(d_lm))
d_plot()
lines(d$date,tmin_,col="blue",lty=2)

##　

# 1965-2001
# 久米さん　黒部ダム2023関西電力.xlsx

m2<-as.matrix(d2[,2:13])
dim(m2)
v2<-as.numeric(t(m2))
plot(c(v2,d$mean),type="l")
str(d2)

plot(d2[,1],apply(d2[,-1],1,mean),type="b")


d2_year_mean<-data.frame(year=ISOdate(d2[,1],6,12),t=apply(m2,1,mean))
points(d2_year_mean$year,d2_year_mean$t,col="green")
### d2 このデータは最大値である　####

as.numeric(t(m2))

tmin_predict<-as.numeric(predict(d_lm,newdata=data.frame(tmax=v2)))
plot(v2,tmin_predict)

# dd データ構築　####
dd<-data.frame(date=seq(ISOdate(1965,1,15),ISOdate(2023,12,15),by="month"),min=NA,max=NA,mean=NA)
dd$date[1]
month(as.POSIXct("2023-09-01 12:00:00 GMT" ))

month(dd$date[1])

as.numeric(substr(dd$date[100],6,7))

##  d2 incert to dd ####
rn_dd <- function(year,month) which(ISOdate(year,month,15)==dd$date)
i1 <- rn_dd (1965,1)
i2<- rn_dd (2001,12)
dd$max[i1:i2]<-as.numeric(t(m2))

dd$min[i1:i2]<-tmin_predict
dd$mean[i1:i2]<-(tmin_predict+dd$max[i1:i2])/2

##  d incert to dd ####
tail(d)
i1 <- rn_dd (2010,2)
i2 <- rn_dd (2023,10)
names(dd)
dd[i1:i2,c("min","max","mean")]<-d[,c("tmin","tmax","mean")]
dd_plot<-function(){
  plot(dd$date,dd$max,type="l",col="red",ylim=c(-10,25))
  lines(dd$date,dd$min,col="blue")
  lines(dd$date,dd$mean,col="green")
}

dd_plot()

##  d4 incert to dd ####
# 立山周辺気温データ解析2023.xlsx "kurobe" 2003-2023
#
d4<-d..
d4_date<-seq(ISOdate(2003,1,15),ISOdate(2023,12,15),by="month")
d4. <- as.numeric(t(d4[,2:13]))

dd_plot()
lines(d4_date,d4.,col="green") #平均値のようだ

i1 <- rn_dd (2003,1)
i2 <- rn_dd (2023,12)
dd$mean[i1:i2]<-d4.
dd_plot()

## 2001 の補完　####
i1 <- rn_dd (2000,1)
i2 <- rn_dd (2003,12)
dd[i1:i2,]
## 2002とはどんな年?
d3[d3$year==2002,]
d3_plot();abline(v=ISOdate(2002,6,15)) #1999とにる
d3[d3$year==1999,]

i1 <- rn_dd (1999,1)
i2 <- rn_dd (1999,12)
dd$mean[rn_dd (2002,1):rn_dd (2002,12)]<-dd$mean[rn_dd (1999,1):rn_dd (1999,12)]
dd_plot()

# save.image(file = "kurobe.RData")

#
m_dd_mean<-matrix(dd$mean,ncol=12,byrow=T)

wi_calc <- function(T12=1:12){
  T12<-as.numeric(T12)
  wi. <- T12-5
  wi.[wi.<0]<-0
  return(sum(wi.))
}

wi_dd_mean<-apply(m_dd_mean,1,wi_calc)
kurobe_dam_wi<-data.frame(year=year=1965:2023,wi_dd_mean)
plot(kurobe_dam_wi)


# 各調査地点の温量指数の経年変化
m_dd_mean
m_dd_mean[59,11:12]<-c(5.2,-3.3)
(dt<--0.55/100*(plt4$alt-1368))

ii<-1
wi1965<-plt4$na
wi.<-c()  #_withKurobeDamObservation
for(ii in 1:nrow(plt4)){
  wi.<-rbind(wi.,apply(m_dd_mean+dt[ii],1,wi_calc))
}
colnames(wi.)<-1965:2023
rownames(wi.)<-plt4$na
WI_with_KurodeDamObservation <- wi.
# save(WI_with_KurodeDamObservation,file="WI_with_KurodeDamObservation.RData")







