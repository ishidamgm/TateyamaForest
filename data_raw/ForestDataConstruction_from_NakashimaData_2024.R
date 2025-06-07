# ForestDataConstruction_from_NakashimaData.R

#　ワーキング・ディレクトリの設定
getwd <- old.wd
wd.<-"~/8T/Dropbox/00D/00/tateyama/TateyamaForest/works/R"
#wd.<-"/home/i/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2023/報告書/!提出分/2023林分_気象データ/02林分調査/2020-2025_7th/R/"
wd.<-"../works/Nakajima/R/"
setwd(wd.)
dir()


##　調査区名とファイル名
dir()
(plot_profile <- read.csv("plot profile 2024.csv",fileEncoding = "shift-jis",skip=1))
fyr <- plot_profile$fyr
na <- plot_profile$na
(pn <- paste0(na,fyr,".csv"))



##　毎木調査データの取り込み
d0<-c()
for(i in 1:length(pn)){
  d0<-c(d0,list(read.csv(pn[i],fileEncoding = "CP932")))
}
names(d0)<- na


# 各調査区計測年　####
## colnames_DBH : column name of DBH for each year ####
colnames_D<-data.frame(matrix(NA,8,7))
rownames(colnames_D) <- na
for (i in 1:nrow(plot_profile)){
  cnam <- names(d0[[i]])
  cnam <- cnam[substr(cnam,1,1)=="D"]
  for(j in 1:length(cnam))  colnames_D[i,j] <-cnam[j]
}
colnames_D

## clm_f : column name of Vitality Index for each year ####
colnames_f<-data.frame(matrix(NA,8,7))
rownames(colnames_f) <- na
for (i in 1:nrow(plot_profile)){
  cnam <- names(d0[[i]])
  cnam <- cnam[substr(cnam,1,1)=="f"]
  for(j in 1:length(cnam))  colnames_f[i,j] <-cnam[j]
}
colnames_f



## yr : survey years ####
yr <- plot_profile[,c("yr1","yr2","yr3","yr4","yr5","yr6","yr7")]
rownames(yr) <- na
yr

!is.na(yr)

# save data ####

TateyamaForest2024 <- list(plot_profile=plot_profile,d0=d0,colnames_D=colnames_D,colnames_f=colnames_f,yr=yr)
names(TateyamaForest2024 )

# save(TateyamaForest2024,file="TateyamaForest2024.RData") ####
