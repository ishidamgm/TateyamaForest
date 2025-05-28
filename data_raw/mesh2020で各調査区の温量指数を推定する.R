
# mesh2020で各調査区の温量指数を推定する.R

library(TateyamaForest)
help(package=TateyamaForest)
data(package="TateyamaForest")

# test ####
d87<-read.csv("Toyama_mesh_1987_original.csv")
d87[match( plt5$コード,d87[,1]),3]  # 981m
plt5                       # 981m
d10<-read.csv("Toyama_mesh_2010_original.csv")
d10[d10[,1]=="54376397",]  # 県境コード、標高(2,3列)はいっていることになっているがない

#　https://nlftp.mlit.go.jp/ksj/gmlold/product_spec/KS-PS-G02-v1_1.pdf


# https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-G04-a.html
# 2011年度（平成23年度）に基盤地図情報を使い世界測地系で新たに作成。
alt2010<-read.csv("G04-a-11_5437-jgd_GML.csv")
head(alt2010)

plt4$alt_
(alt_<-alt2010[match( plt5$コード,alt2010[,1]),2] ) # 1053.6 m

# m2020 の読み込み
plt5$alt_<-alt_
names(plt5)
plt4[,paste0("t",1:12)]
(t.<-plt5[,paste0("平均気温",sprintf("%02d",1:12))]/10-(plt5$alt-plt5$alt_)*0.55/100)
(t.mean<-apply(t.,1,mean))
(wi.<-apply(t.,1,wi_calc))
(ci.<-apply(t.,1,ci_calc))
# plt5 への代入　####

names(plt5)
plt5$WI<-wi.
plt5$CI<-ci.
plt5[,paste0("t",1:12)]<-t.
plt5$t13<-t.mean

# save(plt5,file="plt5.RData")
