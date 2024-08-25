#立山毎木調査_dd_plt.R

dir()
plt <- read.csv("plot profile.csv" ,skip=1)
head(plt)
names(plt)
plt$na
#### データファイル読み込み

datdir <- "./plot"
setwd(datdir)
(f<-dir())

dd<-c(); for(i in 1:nrow(plt))dd<-c(dd,list( read.csv(f[i])))
setwd("../")
dir()
names(dd)<-plt$na
edit(dd[[1]])
names(dd)[5]

####save(plt,dd,file="立山毎木調査_dd_plt.Rdata")
load("立山毎木調査_dd_plt.Rdata")










