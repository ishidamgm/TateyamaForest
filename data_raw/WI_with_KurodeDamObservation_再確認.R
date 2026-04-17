# WI_with_KurodeDamObservation.R
#　~/8T/Dropbox/00D/00/tateyama/TateyamaForest/works/kurobe/黒部ダム月別気温.R
# load("kurobe.RData")   #d(2010-2023), d2(1965-2001), d3(kurobe_dam_temperature)
# save.image(file = "kurobe.RData")

# 2026/4/13 奥野さん(関電)から2023，2024の月データ届く
dd$date <- format(.$date, "%Y/%m/%d")

write.csv(dd, "your_output.csv", row.names = FALSE)
kurobe_1965_2024<-read.csv("黒部ダム集計_1965-2024.csv")
z<-kurobe_1965_2024
z$date <- format(as.POSIXct(z$date, format = "%Y-%m-%d %H:%M:%S"), "%Y/%m/%d")
z$date <- as.Date(z$date)
str(z)
plot(z$date,z$min,type="l")

# write.csv(z,file="data_raw/kurobe_1965_2024.csv")
kurobe_1965_2024<-z
# save(kurobe_1965_2024,file="data/kurobe_1965_2024.RData")

# kurobe_1965_2024_monthly ####
z$date
idx<-substr(z$date,1,7)
tmin.<-tapply(z$min,idx,mean)
tmax.<-tapply(z$max,idx,mean)
tmean.<-tapply(z$mean,idx,mean)
kurobe_1965_2024_monthly<-data.frame(tmin=tmin.,tmax=tmax.,tmean=tmean.)
# write.csv(kurobe_1965_2024_monthly,file="kurobe_1965_2024_monthly.csv")
# save(kurobe_1965_2024_monthly,file="data/kurobe_1965_2024_monthly.RData") ####
idx<-substr(z$date,1,4)
tmin.<-tapply(z$min,idx,mean)
tmax.<-tapply(z$max,idx,mean)
tmean.<-tapply(z$mean,idx,mean)
zy<-data.frame(year=as.numeric(names(tmin.)),min=tmin.,max=tmax.,mean=tmean.)
rownames(zy)<-1:nrow(zy)
kurobe_dam_temperature<-zy
# write.csv(kurobe_dam_temperature,file="kurobe_dam_temperature.csv")
# save(kurobe_dam_temperature,file="data/kurobe_dam_temperature.RData") ####

# 2026/4/12 wi計算方法の確認
#　久米さんのデータ　d　+　奥野さん(関電)　kurobe_dam_temperature
#


#　2026/4/14 久米さんのデータ変更に対する変更　#####

# kurobe_dam_temperature_regression更新 ####
z<-kurobe_dam_temperature_regression
# write.csv(z,file="data_raw/kurobe_dam_temperature_regression_old")

str(z)
write_clip(z, sep = "\t")
z2<-read_clip_tbl()
z3<-z2
z3[is.na(z3)]<-""
kurobe_dam_temperature_regression<-z3
# save(kurobe_dam_temperature_regression,file="data/kurobe_dam_temperature_regression.RData")


####################################
library(lubridate)
help(package=lubridate)
#　スプレッドシートのコピペ　clipr　d.. ; Kurobe.Dam 　2003-2011　####
library(clipr)
#　d..<-read.table(text = read_clip(), header = TRUE, sep = "\t")
# タブ区切りでクリップボードにコピー
#　write_clip(df, sep = "\t")
# Kurobe.Dam         X       X.1        X.2      X.3       X.4      X.5      X.6      X.7
# 1        2003 -6.258065 -5.339286 -3.3870968 5.500000



# 1965-2023　各年　2024年奥野さんから貸与されたデータ　d3　####
d3<-kurobe_dam_temperature
d3$date <- ISOdate(d3$year,6,15)
d3_plot<-function(){
  plot(d3$date,d3$min,ylim=c(-0,15),type="b")
  points(d3$date,d3$mean,col="black",type="b")
  points(d3$date,d3$max,col="red",type="b")
}

d3_plot()

plot(d3$min,d3$max)


# 2010-2023　各月　####
# 久米さん　黒部ダム2023関西電力.xlsx　d: 2010-2023 ####
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

data.frame(tmain=tapply(d$tmin,d$year,mean),tmax=tapply(d$tmax,d$year,mean))
names(d)

# d(2010-2023 monthly) d3(1965-2023 yearly ) d2 (1965-2001 monthly) 確認　#####
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

##  d2 insert to dd ####
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
dd_plot<-function(dd=dd){
  plot(dd$date,dd$max,type="l",col="red",ylim=c(-10,25),
       xlab="year",ylab="Temperature at Kurobe dam")
  lines(dd$date,dd$min,col="blue")
  lines(dd$date,dd$mean,col="green")
}

dd_plot(dd)
# 2001-2009 挿入　#########
dayly1997_2009<-read.csv("kurobe_dayly_1997-2009.csv")
.<-dayly1997_2009
plot(.$tmax,.$tmin)
idx<-paste(dayly1997_2009$year,sprintf("%02d", dayly1997_2009$month),sep="_")
month1997_2009_tmin<-tapply(dayly1997_2009$tmin,idx,mean)
month1997_2009_tmax<-tapply(dayly1997_2009$tmax,idx,mean)
month1997_2009_tmin_tmax<-data.frame(tmin=month1997_2009_tmin,tmax=month1997_2009_tmax)
names(month1997_2009_tmin_tmax)<-c("tmin","tmax")
month1997_2009_tmin_tmax
?sprintf
head(dd)

dd2<-dd
i<-541
dd2[i,]
dd2$min[i]<--9.7 ;dd2$max[i]<--1.2
tmin.<-month1997_2009_tmin_tmax$tmin
tmax.<-month1997_2009_tmin_tmax$tmax
tmean.<-(tmin.+tmax.)/2
i<-385:540
dd2$min[i]<-tmin. ;dd2$max[i]<-tmax.;dd2$mean[i]<-tmean.
dd_plot(dd2)
# write.csv(dd2,file="kurobe_1965-2023.csv")
yr_tmax<-tapply(dd2$max,substr(dd2$date,1,4),mean)
yr_tmin<-tapply(dd2$min,substr(dd2$date,1,4),mean)
dd2.yr<-data.frame(year=as.numeric(rownames(dd2.yr)),tmin=yr_tmin,tmax=yr_tmax,tmean=(yr_tmin+yr_tmax)/2)
(res_tmmin<-summary(lm(dd2.yr$tmin~dd2.yr$year,na.rm=T)))
(res_tmax<-summary(lm(dd2.yr$tmax~dd2.yr$year,na.rm=T)))
(res_tmean<-summary(lm(dd2.yr$tmean~dd2.yr$year,na.rm=T)))

plot(dd2.yr$year,dd2.yr$tmax,ylab="Temperature",type="b",col="red",ylim=c(-1,13))
lines(dd2.yr$year,dd2.yr$tmin,ylab="Temperature",type="b",col="blue")
lines(dd2.yr$year,dd2.yr$tmean,ylab="Temperature",type="b",col="black")

# パターンが同じすぎる -　一部回帰式使われている ####
.<-dd2.yr
plot(.$tmin,.$tmax)
i<-1:32
points(.$tmin[i],.$tmax[i],col="red",cex=2)
text(.$tmin,.$tmax,.$year)

#　入れ替え検討　黒部総合_日最低気温_1965_2001　久米さん######
min.<-read.csv("黒部総合_日最低気温_1965_2001.csv")
max.<-read.csv("黒部総合_日最高気温_1965_2001.csv")
dd2.<-dd2
min..<-as.matrix(min.[,2:13])
min_1965_2001<-t(min..)[1:length(min..)]
max..<-as.matrix(max.[,2:13])
max_1965_2001<-t(max..)[1:length(max..)]
plot(min_1965_2001,max_1965_2001)
length(max_1965_2001)
##
names(dd2.)
i<-1:444
dd2.$min[i]<-min_1965_2001
dd2.$max[i]<-max_1965_2001
dd2.old<-dd2
dd2<-dd2.

(tstamp<-seq(ISOdate(2023,11,15),ISOdate(2024,12,15),by="month"))
write_clip(tstamp)


# write.cdd2.old# write.csv(dd2.,file="kurobe_1965-2023.csv")
##  d4 insert to dd ####
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

#欠測年　2003,2004,2014
yr<-as.numeric(rownames(WI_with_KurodeDamObservation))
diff(yr)
setdiff(1965:2024,yr)







