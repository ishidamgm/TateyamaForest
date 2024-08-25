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


# data bind ####
dd2 <-c()
for(ii in 1:length(dd)){#length(dd)
  dd2 <-rbind(dd2,data.frame(plot=ii,dd[[ii]]))
  print(ii)
  print(names(dd[[ii]]))
}
nrow(dd2) #edit(dd2)

####save(plt,dd,dd2,file="立山毎木調査_dd_dd2_plt.Rdata")
load("立山毎木調査_dd_dd2_plt.Rdata")










