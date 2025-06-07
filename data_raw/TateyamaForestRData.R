# TateyamaForestRData.R
# rm(list=ls())
# ls()
getwd()
fs::dir_tree()

library("TateyamaForest")
data(package="TateyamaForest")
help(package="TateyamaForest")
dir("data/")

# 20231127時点で美松と松尾峠のデータが入れ替わっている　#####
d.<-subset(dd3,plot=="Mimatsu")
head(d.) # 6 Mimatsu
plot(d.$x,d.$y)

d.<-subset(dd3,plot=="Matsuotoge")
head(d.) #  5 Matsuotoge
plot(d.$x,d.$y)
plt
dd3<-d.
#

#save(dd3,plt,file="./data/TateyamaForest_dd3_plt.RData")

# 松尾峠　⇒　美松の順で来ていたが　標高が松尾峠の方が高いということで逆にしたことがあった
#　従来通り　松尾峠(5[6])　⇒　美松(6[7])にする
#　pltはそのまま、dd3のデータ

d.<-dd3
d.$plot[dd3$plot=="Matsuotoge"]="Mimatsu"
d.$plot[dd3$plot=="Mimatsu"]="Matsuotoge"
d.$pn[d.$plot=="Matsuotoge"]=5
d.$pn[d.$plot=="Mimatsu"]=6
head(subset(d.,plot=="Mimatsu"))
head(subset(d.,plot=="Matsuotoge"))
plt
##
if(0){
  clm_f <- c("f02","f03","f04","f05","f06")
  clm_dbh <- c("d02","d03","d04","d05","d06")
  clm_yr <- c("yr2","yr3","yr4","yr5","yr6")

  #save(vital.clm,dbh.clm,yr.clm,file="../data/clm.RData") ####
}


# save(dd,dd2,file="data/TateyamaForest_dd_dd2.Rdata")

# species name from Jp to Roman ####
if(0){
  dd3<-dd2
  dd3$sp<-stringi::stri_trans_general(dd3$sp, "Any-latn")
  edit(dd3)
  dir("./data")
  #save(dd3,plt,file="TateyamaForest_dd3_plt.RData")
}


if(0){
  # add plot name and leg (legend of plots) 20231107 ####
  dir("../data")
  load("../data/leg.RData")
  plt<-data.frame(plt,leg)
  #save(plt,file="plt.RData")
}


if(0){
  # add plot name to dd3 20231107 ####

  dir("../data")
  load("../data/TateyamaForest_dd3_plt.RData")
  load("../data/TateyamaForest_dd_dd2.RData")
  names(dd3)
  head(dd3)

  pn<-dd3$plot
  i<-match(dd3$plot,plt$no)
  dd3$plot <- plt$na[i]
  dd3<-data.frame(pn,dd3)

  # save(dd3,plt,file="./data/TateyamaForest_dd3_plt.RData")

}

# legend ####
leg<-data.frame(n=plt$na,
                col=c("brown1","darkolivegreen3","darkolivegreen4","blueviolet","blue","blue4","cyan3","chartreuse3"),
                pch=c(4,13,13,11,17,2,8,1),
                lty=c(2, 1, 2, 2, 2,1,2,2)
)
# save(leg,file="./data/leg.RData")




