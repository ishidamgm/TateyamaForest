v<-c(1,2,NA,3)
v==NA
is.na(v)



x
mean(d$D16,na.rm=T)


dbh<-d$D16
sp<-d$sp
mean(dbh[!is.na(dbh)])

mean(dbh,na.rm=T)


i<-!is.na(dbh)
tapply(d[i,"D16"],d[i,]$sp,mean)

d[i,]$D16

d[i,c("sp","D16")]
par(mfrow=c(2,2))
hist(dbh[i])
hist(dbh[sp[i]=="オオシラビソ"])
hist(dbh[sp[i]=="ナナカマド"])

#####
dir()
p<-read.csv( "plot profile.csv" ,skip=1)
names(p)
plot(p$alt)
text(p$alt,p$na)


####
ba<-pi*(dbh/200)^2
ba
plot(dbh[i],ba[i])
sum(ba[i][sp[i]=="オオシラビソ"])/sum(ba[i])
sum(ba[i][sp[i]=="ナナカマド"])/sum(ba[i])

#####
#####
dir()
bijo<-read.csv("Bijodaira2016.csv")
names(bijo)
dbh<-bijo$D16
sp<-bijo$sp
i1<-!is.na(dbh1)
tapply(bijo[i,"D16"],bijo[i,]$sp,sum)
ba<-pi*(dbh/200)^2
ba
plot(dbh1[i1],ba[i1])
sum(ba[i1][sp[i1]=="スギ"])/sum(ba[i1])
#####
dir()
bz<-read.csv("Bunazaka2015.csv")
names(bz)
dbh<-bz$D15
sp<-bz$sp
i<-!is.na(dbh)
tapply(bz[i,"D15"],bz[i,]$sp,sum)
ba<-pi*(dbh/200)^2
ba
plot(dbh[i],ba[i])
sum(ba[i][sp[i]=="スギ"])/sum(ba[i])

#####
dir()
bijo<-read.csv("Bijodaira2016.csv")
names(bijo)
dbh<-bijo$D16
sp<-bijo$sp
i<-!is.na(dbh)
tapply(bijo[i,"D16"],bijo[i,]$sp,sum)
ba<-pi*(dbh/200)^2
ba
plot(dbh[i],ba[i])
sum(ba[i][sp[i]=="スギ"])/sum(ba[i])
#####
dir()
bijo<-read.csv("Bijodaira2016.csv")
names(bijo)
dbh<-bijo$D16
sp<-bijo$sp
i<-!is.na(dbh)
tapply(bijo[i,"D16"],bijo[i,]$sp,sum)
ba<-pi*(dbh/200)^2
ba
plot(dbh[i],ba[i])
sum(ba[i][sp[i]=="スギ"])/sum(ba[i])
#####
dir()
bijo<-read.csv("Bijodaira2016.csv")
names(bijo)
dbh<-bijo$D16
sp<-bijo$sp
i<-!is.na(dbh)
tapply(bijo[i,"D16"],bijo[i,]$sp,sum)
ba<-pi*(dbh/200)^2
ba
plot(dbh[i],ba[i])
sum(ba[i][sp[i]=="スギ"])/sum(ba[i])
#####
dir()
bijo<-read.csv("Bijodaira2016.csv")
names(bijo)
dbh<-bijo$D16
sp<-bijo$sp
i<-!is.na(dbh)
tapply(bijo[i,"D16"],bijo[i,]$sp,sum)
ba<-pi*(dbh/200)^2
ba
plot(dbh[i],ba[i])
sum(ba[i][sp[i]=="スギ"])/sum(ba[i])









