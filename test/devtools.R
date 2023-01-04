rm(list=ls())
devtools::install_github("ishidamgm/TateyamaForest")
library(TateyamaForest)
help(package="TateyamaForest")
data(package="TateyamaForest")
dd3
dd2
clm_f
plt[,clm_yr]
TateyamaForest::

f<-function(){
  return(TateyamaForest::dd2)
}
f()

plot. <- "Kaminokodaira"
sp.1 <- "ooshirabiso"
sp.2 <- "buna"
ba.sp1 <- BasalArea_1_6(plot.,sp.1)
ba.sp2 <- BasalArea_1_6(plot.,sp.2)
Year <- ba.sp1$Year
rba.sp1 <- 100*ba.sp1[,2]/ba.sp1[1,2]   #relative basal area
rba.sp2 <- 100*ba.sp2[,2]/ba.sp2[1,2]   #relative basal area
plot(Year,rba.sp1,ylab="Basal area (%)", ylim=c(60,200),
     type="b",lwd=5,pch=2,col="blue", main="Kaminokodaira (a.s.l. 1450m　ecotone)")
lines(Year,rba.sp2,type="b",col="orange",lwd=5)
abline(h=100,col="red",lty=2,lwd=2)
