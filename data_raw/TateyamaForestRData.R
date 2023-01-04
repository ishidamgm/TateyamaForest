# TateyamaForestRData.R
data(package="TateyamaForest")
dir("data/")
##
clm_dbh=c("d01","d02","d03","d04","d05","d06")
clm_f=c("f01","f02","f03","f04","f05","f06")
clm_yr = c("yr2","yr3","yr4","yr5","yr6")
#save(clm_dbh,clm_f,clm_yr,file="data/clm.RData")

# save(dd,dd2,file="data/TateyamaForest_dd_dd2.Rdata")
rm(list=ls())
load("data/TateyamaForest_dd_dd2.Rdata")
ls()
