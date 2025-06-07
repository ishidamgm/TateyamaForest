# TateyamaForestRData.R
# rm(list=ls())
# ls()
getwd()
fs::dir_tree()

library("TateyamaForest")
data(package="TateyamaForest")
help(package="TateyamaForest")
dir("data/")


# 2024/10/21(日) 14:29 dd3に6期までのデータしか無い
# 【案1】TateyamaForest2024 のd0とplt3からdd4を作成し dd4.Rdataとして保存
# 【案2】dd3に追加する
#　とりあえずバックアップ

table(dd3$plot)
.<-TateyamaForest2024
d0<-.$d0
sapply(d0,nrow)
# Kaminokodairaがかなり違う
#　中島さんオリジナルのd0から一撃でdd4を作るプログラムを作成するのが無難と思う
names(.)
ii<-1
data.frame(names(dd3))
unique(dd3$pn)
sapply(d0,names)

names(dd3)
# dd4 : A data frame for all plots field surveys ####
itm <- c("pn","plot","lb","sp","d01","d02","d03","d04","d05","d06","d07","f01","f02","f03","f04","f05","f06","f07","x", "y")
dclm <- .$colnames_D
fclm <- .$colnames_f
na   <- plt$na[ii]

dd4 <-data.frame()
for(ii in 1:nrow(plt)){　# ii<-6
  na. <- plt$na[ii]
  d0. <- d0[[na.]] #  names(d0.)
  itm. <- c("lb","sp",as.character(c(dclm[ii,],fclm[ii,])),"x","y")
  ###  term 7 measurements have not carried out in Mimatsu, Kagamiishi and Arimine plots, at 2024. ####
    if(is.element(na.,c("Mimatsu","Kagamiishi","Arimine"))){
      d0.<-data.frame(d0.,d07=NA,f07=NA)
      itm.[is.na(itm.)] <- c("d07","f07")
    }
  # names(d0.)
  d. <- data.frame(pn=ii,plot=na.,d0.[,itm.])

  names(d.) <- itm
  dd4 <- rbind(dd4,d.)
}

dd4

## save(dd4,file="data/dd4.RData") ####

# 美松と松尾峠のデータ入れ替え　####
# 20231127時点で美松と松尾峠のデータが入れ替わっていた　
#　標高が美松のほうが高いため小川くんの卒論の際には順番を入れ替えた
#　plotの名前でデータ取得するのが無難

if(0){
  #　樹木位置図で確認
  d.<-subset(dd3,plot=="Mimatsu")
  head(d.) # 6 Mimatsu
  plot(d.$x,d.$y)

  d.<-subset(dd3,plot=="Matsuotoge")
  head(d.) #  5 Matsuotoge
  plot(d.$x,d.$y)
  plt
  dd3<-d.
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

  ##save(dd3,plt,file="./data/TateyamaForest_dd3_plt.RData") ####
}



#　コラムname　####
if(0){
  clm_f <- c("f02","f03","f04","f05","f06")
  clm_dbh <- c("d02","d03","d04","d05","d06")
  clm_yr <- c("yr2","yr3","yr4","yr5","yr6")

  ##save(vital.clm,dbh.clm,yr.clm,file="../data/clm.RData") ####
}

# TateyamaForest_dd_dd2.Rdata ####
## save(dd,dd2,file="data/TateyamaForest_dd_dd2.Rdata") ####

# species name from Jp to Roman ####
if(0){
  dd3<-dd2
  dd3$sp<-stringi::stri_trans_general(dd3$sp, "Any-latn")
  edit(dd3)
  dir("./data")
  ## save(dd3,plt,file="TateyamaForest_dd3_plt.RData") ####
}

# sp_dbh() ####
. <- TateyamaForest2024
d0=.$d0; plt=.$plot_profile;cnD=.$colnames_D;cnf=.$colnames_f;yr=.$yr
cls<-seq(0,300,10)
sp_dbh <- c()
for (ii in 1:nrow(plt)){
  sp_dbh.<- table(d0[[ii]]$sp,cut(d0[[ii]][,cnD[ii,1]],cls))
  colnames(sp_dbh.)<-cls[-1]
  sp_dbh.<-sp_dbh.[,1:which.max(cumsum(colSums(sp_dbh.)))]
  sp_dbh <- c(sp_dbh,list(sp_dbh.))
}

# '------------------------------' ####
# SpeciesList SpeciesList2 ####
sp0 <- read.csv("../works/第5期植物目録.csv")
names(sp0)
sp0[,c("美女","ブナ坂","ブナ平","上の小","美松" ,"松尾峠","鏡石","有峰") ]
sp0$spj[sp0$form==""]

sp. <- c()
for (ii in 1:nrow(plt)){
  sp. <- c(sp.,rownames(sp_dbh[[ii]]))
}
spj<-unique(sp.)
i<-match(spj,sp0$spj)
id<-sp0$id[i]
form <-sp0$form[i]
sp_full <- sp0$sp[i]
sp <- sub("^([^ ]+ [^ ]+).*", "\\1", sp_full)
## species type of distribution zone  c("Temperate","Ecotone","Subarctic") ####
sp <- data.frame(id,spj,sp,form,zone="Temperate",sp_full)
sp$zone[match(c("キタゴヨウ","クロベ","ネコシデ"),sp$spj)]<-"Ecotone"
sp$zone[match(c("オオシラビソ","ダケカンバ","ミネカエデ"),sp$spj)]<-"Subarctic"
SpeciesList<-sp[order(id),]
rownames(SpeciesList)<-1:nrow(SpeciesList)
SpeciesList
SpeciesList2 <- SpeciesList[,-6]
## write.csv(sp,file="data/SpeciesList.csv") ####
## save(SpeciesList,file="data/SpeciesList.RData") ####
## save(SpeciesList2,file="data/SpeciesList2.RData") ####


#　SpeciesList1 : 各調査区の期間中の平均出現頻度を追加 ####
#　
bar.<-sapply(sp_ba_ratio,rowMeans)
sp.<-SpeciesList2
sp.<-data.frame(sp.,"Bijodaira"=0,"Bunazaka"=0,"Bunadaira"=0,"Kaminokodaira"=0,"Matsuotoge"=0,"Mimatsu"=0,"Kagamiishi"=0,"Arimine"=0)
for(ii in 1:length(bar.)){#ii<-1
  .<-bar.[[ii]]
  sp.[match(names(.),sp.$spj),names(bar.)[ii]]<-round(.,8)
}
sp.
sp.$spj[rowSums(sp.[,6:13])==0]  #　除外　"マルバマンサク" "ツリバナ"
sp..<-sp.[!rowSums(sp.[,6:13])==0,]
colSums(sp.[,6:13])
rownames(sp..)<-1:nrow(sp..)
SpeciesList1 <- sp..
SpeciesList1
## save(SpeciesList1,file="data/SpeciesList1.RData") ####

#  SpeciesList2 : intact plots ####
#　人為的影響の少ない立山プロット "Bijodaira"     "Bunazaka"      "Kaminokodaira" "Matsuotoge"    "Kagamiishi"
# でdbh10cm以上の調査対象とならなかった種
i <- which(rowSums(sp..[,plt2$na])==0)
sp..$spj[i]
# 除外　"ミヤマハンノキ" "オノエヤナギ"   "リョウブ"       "コミネカエデ"   "オガラバナ"     "ヤマウルシ"
sp...<-sp..[-i,-c(6,8,11,13)]
rownames(sp...)<-1:nrow(sp...)
sp...
SpeciesList2 <- sp...
SpeciesList2
## save(SpeciesList2,file="data/SpeciesList2.RData") ####


# plt : add plot name and leg (legend of plots) 20231107 ####
if(0){
  dir("../data")
  load("../data/leg.RData")
  plt<-data.frame(plt,leg)
## save(plt,file="plt.RData") ####
}

# TateyamaForest_dd3_plt : add plot name to dd3 20231107 ####
if(0){
  dir("../data")
  load("../data/TateyamaForest_dd3_plt.RData")
  load("../data/TateyamaForest_dd_dd2.RData")
  names(dd3)
  head(dd3)

  pn<-dd3$plot
  i<-match(dd3$plot,plt$no)
  dd3$plot <- plt$na[i]
  dd3<-data.frame(pn,dd3)

## save(dd3,plt,file="./data/TateyamaForest_dd3_plt.RData") ####

}

# '------------------------------' ####



names(sp_dbh) <- plt$na

sp_dbh

## save(sp_dbh,file="data/sp_dbh.RData")

# leg : legend ####
leg<-data.frame(n=plt$na,
                col=c("brown1","darkolivegreen3","darkolivegreen4","blueviolet","blue","blue4","cyan3","chartreuse3"),
                pch=c(4,13,13,11,17,2,8,1),
                lty=c(2, 1, 2, 2, 2,1,2,2)
)
## save(leg,file="./data/leg.RData") ####






# plt2 : intact plot ####
# 人為的撹乱の影響の多い　美女平、ブナ平、美松、有峰を除外した取りまとめについて考える。
# dd3のpltとTateyamaForest2024$plot_prophileがダブっているので整理する必要ある
# .RDataに複数のデータを入れること　
# listにしておくと取り出しが面倒
# 複数入れておくとオブジェクト名が直ちにわかりにくい　ファイル名を工夫するなど
# TateyamaForest2024にplt2としてappendする。これまでのスクリプトに影響しない
# ⇒現実的な方法か?

plt2<-TateyamaForest2024$plot_profile[-c(1,3,6,8),]
plt2
TateyamaForest2024$plt2<-plt2
names(TateyamaForest2024)
## save(TateyamaForest2024,file="data/TateyamaForest2024.RData") ####
## save(plt2,file="data/plt2.RData") ####


## save(plt3,file="plt3.RData") ####


# '------------- Basal Area -----------------' ####

#　ba_mean_ha ####
l. <-c()
for (ii in 1:length(sp_ba)){#ii<-5
  .<-sp_ba[[ii]]
  m.<-rowMeans(.)/plt$Area[ii]*10000
  d. <- data.frame(pn=ii,ba_ha=m.,ba_paecent=round(100*m./sum(m.),10))
  l.<-c(l.,list(d.))
}
names(l.)<-names(sp_ba)
ba_mean_ha <- l.
### save(ba_mean_ha,file="data/ba_mean_ha.RData") ####


# sp_zone_ba_ratio  : sp_zone_ba_ratio_calc() ####　
(sp_zone_ba_ratio<-lapply(1:7,sp_zone_ba_ratio_calc))
## save(sp_zone_ba_ratio,file="data/sp_zone_ba_ratio.RData") ####

sp_zone_ba_ratio


d. <-c()
for (term in 1:7){
  d. <- c(d.,list(sp_zone_ba[[term]]+sp_zone_ba_dead[[term]]))
}

(sp_zone_ba_live_dead <- d.)

## save(sp_zone_ba_live_dead,file="data/sp_zone_ba_live_dead.RData") ####

# '-------------   sp_zone_ba_live_dead2  -----------------' ####
# bind sp_zone_ba &  sp_zone_ba_dead

sp_zone_ba_live_dead2<-c()
for (term in 1:7){
  d.<-data.frame(sp_zone_ba[[term]],sp_zone_ba_dead[[term]])
  d.<-d.[,c(1,4,2,5,3,6)]
  names(d.)<-gsub("1","dead",names(d.))
  sp_zone_ba_live_dead2  <- c(sp_zone_ba_live_dead2,list(d.))
}

sp_zone_ba_live_dead2

## save(sp_zone_ba_live_dead2,file="data/sp_zone_ba_live_dead2.RData") ####

# '-------------   sp_zone_ba_live_dead2_ratio  -----------------' ####
sp_zone_ba_live_dead2_ratio <- lapply(sp_zone_ba_live_dead2,function(x)x/rowSums(x))

## save(sp_zone_ba_live_dead2_ratio,file="data/sp_zone_ba_live_dead2_ratio.RData") ####

# '-------------- Warmth Index ----------------' ####

# old ToyamaMeshData.RData 気候値メッシュファイルのデータ　####
if(0){
  ### これは古い
  #d <- read.csv("../works/富山地理情報_雪氷(1953-1982).csv")
  names(d)
  head(d<-d[,c(1:4,8:20)])
  names(d)<-c("code","lat","lon","alt",paste0("t",1:13))
  head(d)
  plot(d$lon,d$lat)
  ToyamaMeshData<-d
  ## save(ToyamaMeshData,file="data/ToyamaMeshData.RData") ####
}


# new ToyamaMeshData.RData   base year 1987  wgs84 ####
### see https://nlftp.mlit.go.jp/ksj/gmlold/datalist/gmlold_KsjTmplt-G02.html

d <- read.csv("../works/気候値メッシュファイル/Toyama_mesh_1987.csv")
  code <- d[,1]
  LL<-M3CodeToLatLon(code)
  alt_ <- as.numeric(d$G02_003)
  t <- d[,match(paste0("G02_0",seq(21,58,3)),names(d))]　#　Mean temperature
  t[]<-lapply(t, as.numeric)
  t<-t/10

d <- data.frame(code,LL,alt_,t)
names(d)<-c("code","lat","lon","alt_",paste0("t",1:13))
head(d)
ToyamaMeshData_1987 <- d
### save(ToyamaMeshData_1987,file="data/ToyamaMeshData_1987.RData") ####

# plt3  append 7 terms WI data to plt2 ####
d <- ToyamaMeshData_1987
code. <- LLToM3Code(plt2$ido,plt2$kei)
i <- match(code.,d$code)
mesh.<-d[i,]
plt3 <- data.frame(plt2,mesh.)

#### temperature correct for altitude (-0.55/100m) ####
j<-match(paste0("t",1:12),names(plt3))
t <- plt3[,j]
t.<-(t-0.55*(plt3$alt-plt3$alt_)/100)
plt3[,match(paste0("t",1:13),names(plt3))] <- t.
plt3
#### wi test calc ####
wi. <- plt3[,match(paste0("t",1:12),names(plt3))]
wi. <- wi.-5
wi.[wi.<0]<-0
wi.
rowSums(wi.)

# plt3  :   plt2+wi1~7 ####
yr.col<-match(paste0("yr",1:7),colnames(plt2))
plt2[,yr.col]
wi7<-matrix(0,4,7)                  # wi matrix of intact plots
colnames(wi7)<-paste0("wi",1:7)
#
match(plt2$na,colnames(wi_year))

for(ii in 1:nrow(wi7)){
  wi7[ii,]<-wi_year[match(plt2[ii,  yr.col],rownames(wi_year)),match(plt2$na[ii],colnames(wi_year))]
}

wi7


(plt3 <- cbind(plt3,wi7))

## save(plt3,file="data/plt3.RData") ####

# yr_wi_bar.RData ####

yr.wi <- c()
for(ii in 1:nrow(plt3)){
  wi.col <- match(paste0("wi",1:7),colnames(plt3))
  yr <- as.numeric(plt3[ii,yr.col])
  wi <- as.numeric(plt3[ii,wi.col])
  yr.wi. <- data.frame(na=plt3$na[ii],term=1:7,yr,wi,Temperate=0, Ecotone=0,  Subarctic=0)
  yr.wi <- rbind(yr.wi,yr.wi.)

}

yr.wi

Fig_year_WI()
points(yr.wi[,c("yr","wi")],col="red")

zone.ba.ratio<-c()
for(term in 1:length(sp_zone_ba_ratio)){
  ba.<-sp_zone_ba_ratio[[term]]
  ba.<-ba.[match(plt2$na,rownames(ba.)),]
  zone.ba.ratio<-rbind(zone.ba.ratio,ba.)

}
zone.ba.ratio
i<-seq(1,25,4)
zone.ba.ratio<-zone.ba.ratio[c(i,i+1,i+2,i+3),]
yr_wi_bar <- data.frame(na=rep(plt3$na,each=7),yr=yr.wi$yr,wi=yr.wi$wi,zone.ba.ratio,row.names = NULL)
yr_wi_bar
## save(yr_wi_bar,file="data/yr_wi_bar.RData") ####

# WI.RData : calculation WI ####
#"立山植生モニタリング調査_カシミール3D_Tokyo座標系.csv"
# d2<- read.csv("data_raw/立山植生モニタリング調査_カシミール3D_Tokyo座標系.csv")
####### 古いバージョンはTokyo座標系　
###　気候値メッシュファイル2000に変更　基準年 : 1987,  CRS : wgs84
# plt3_1967<-plt3 ; save(plt3_1967,file="../works/plt3_1967.RData")   # 古いデータのバックアップ
# d2<- TateyamaForestMeshData
# d3<-c()
# for (ii in 1:nrow(d2)){
#   #ii<-4
#   lat.<- d2$lat[ii] ; lon.<- d2$lon[ii] ;alt. <- d2$alt[ii]
#   # points(lon.,lat.,col="red",cex=2,pch=3)
#
#   #mesh_data. <- mesh_data(lat.,lon.)
#   temp_correct. <- ( mesh_data.$alt - alt.) * 0.55/100
#   temp. <- mesh_data. [1,5:16] +temp_correct.
#   wi<-sum(temp.[temp. >= 5]-5)
#   #d2$WI[ii]<-sum(temp.[temp. >= 5]-5)
#   d3<-rbind(d3,data.frame(temp.,wi))
# }
#
# d3<-data.frame(d2,d3)
# d3
# WI <- d3
# WI_1967<-WI ; save(WI_1967,file="../works/WI_1967.RData")   # 古いデータのバックアップ
## save(WI,file="data/WI.RData") ####

# wi_year : 黒部ダムの気象観測値をもとにWIの経年変化を推定する　####
# (旧)　1953-1982 基準年　1967年として各調査地点の温量指数の経年変化を推定
# (新)　1973-2002 基準年　1987年として各調査地点の温量指数の経年変化を推定

kurobe_dam_temperature
kurobe_dam_temperature_regression
F_kurobe_dum_mean_tmp <-function(year) -47.932410 +   0.027340 *year
year<-1960:2023
plot(year,F_kurobe_dum_mean_tmp(year))
#　year00<-1953+14  # base year 1967
　　year00<-1987  # base year 1967
abline(v=year00,col="red")


## 平均気温変化率　####
F_kurobe_dum_mean_tmp(year00)
F_kurobe_dum_mean_tmp_diff <-function(year) F_kurobe_dum_mean_tmp(year)-F_kurobe_dum_mean_tmp(year00)
plot(year,F_kurobe_dum_mean_tmp_diff(year))
abline(v=year00,h=0,col="red")

## 温量指数経年変化 ####
names(plt3)
tm00<-plt3[,match(paste0("t",1:13),names(plt3))]   # base year 1987
tm00+F_kurobe_dum_mean_tmp_diff(1960) # example. 1960

wi_year<-c()
year_range<-1960:2024
for (i in year_range){
  tm. <- tm00+F_kurobe_dum_mean_tmp_diff(i)
  wi_year <- rbind(wi_year,apply(tm.,1,function(x)sum(x[x >= 5]-5)))

}
#colnames(wi_year)<-c("Bijodaira","Bunazaka","Bunadaira","Kaminokodaira","Midagahara","Matsuotoge","Mimatsu","Kagamiishi","Joudo","Arimine") #d3$plot
colnames(wi_year)<-plt3$na
rownames(wi_year)<-year_range
wi_year

## save(wi_year,file="data/wi_year.RData") ####



