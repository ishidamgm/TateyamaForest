# ForestDataConstruction_from_NakashimaData.R
wd<-"~/8T/Dropbox/00D/00/tateyama/TateyamaForest/test"
wd.<-"/home/i/8T/Dropbox/00D/00/立山植生モニタリング事業/第05期/2023/報告書/!提出分/2023林分_気象データ/02林分調査/2020-2025_7th/R/"
setwd(wd.)
dir()
pn <-   c("Bijodaira2021.csv","Bunazaka2020.csv","Bunadaira2023.csv" ,
   "Kaminokodaira2018.csv","Matsuotoge2022.csv" ,"Mimatsu2019.csv", "Kagamiishi2022.csv", "Arimine2019.csv")
d0<-c()
for(i in 1:length(pn)){
  d0<-c(d0,list(read.csv(pn[i],fileEncoding = "CP932")))
}
names(d0)<-substr(pn,1,5)
nrow(d0[[5]])
# d5<-read.csv("Matsuotoge2022.csv",fileEncoding = "shift-jis")
# d5<-read.csv("Matsuotoge2022.csv",fileEncoding = "CP932")
# d5<-read.csv("Matsuotoge2022_utf8.csv")

plt$na

dd3.<-c()
for (ii in 1:length(pn)){
  d<-d0[[ii]]
  cnam<-names(d)
  Dc<-cnam[substr(cnam,1,1)=="D"][1:6]
  fc<-cnam[substr(cnam,1,1)=="f"][1:6]
  d.<-data.frame(pn=ii,plot=plt$na[ii],d[,c("lb","sp",Dc,fc,"x","y")])
  names(d.)<-c("pn","plot","lb","sp","d01","d02","d03","d04","d05","d06","f01","f02","f03","f04","f05","f06","x", "y")
  dd3.<-rbind(dd3.,d.)
}
# check data ####
ii=2
head(subset(dd3,pn==ii))
head(subset(dd3.,pn==ii))

tail(subset(dd3,pn==ii))
tail(subset(dd3.,pn==ii))

###############　load("dd3..RData")
d..<-data.frame(old=c(),new=c())
for (ii in 1:length(pn)){
  d.<-subset(dd3,pn==ii)
  old<-sum(pi*(d.$d01/200)^2,na.rm=T)
  d.<-subset(dd3.,pn==ii)
  new<-sum(pi*(d.$d01/200)^2,na.rm=T)
  #d..$old[ii]=old
  #d..$new[ii]=new
  d..[ii,1]=old
  d..[ii,2]=new
  }
  d..

  d.<-subset(dd3.,pn==5)

  hist(d.$d01)

#### setwd(wd) ; save(dd3.,file="dd3..RData")
####   dd3<-dd3. ;  save(dd3,plt,file="../data/TateyamaForest_dd3_plt.RData")

ii<-6
d.<-subset(dd3.,pn==ii)

# Calculates basal areas of each period ####
#'
#' omit tree data with is.na(dbh) and dbh < 10cm
#'
#' @param d   forest data.
#' @param sp  Roman species name. if default is sp="", that operate for all species
#'
#' @return　vector of sum of basal area for each period(year)
#' @export
#'
#' @examples
#' plt
#' plot.<-"Mimatsu"
#' d<-subset(dd3,plot==plot.) #d<-subset(dd3.,pn==4) #
#' d<-d[order(d$lb),]
#' BA_calc(d,"sugi")
#' (ba.<-BA_calc(d,""))
#' ba./ba.[1]
BA_calc <-function(d,sp=""){　#sp="buna"
  i<-if(sp==""){1:nrow(d)}else{d$sp==sp} # 種の指定が""の時全種
  dbh<- d[i,clm_dbh]
  f<-d[i,clm_f] ;f[is.na(f)]<--999

  if(0){
    dbh[dbh<10]<-0
    ba <- pi*(dbh/200)^2
    live<-f ; live[f>0]<-1 ; live[f<=0]<- 0
    #c(sum(f>0),sum(f<=0),sum(live),sum(!is.na(f)))
    ba.live <- ba * live
    ba.sum<-colSums(ba,na.rm=TRUE)
    ba.sum/ba.sum[1]
    }

  dbh[dbh<10 | f<1 ]<-0
  ba <- pi*(dbh/200)^2
  ba.sum<-colSums(ba,na.rm=TRUE)
  ba.sum/ba.sum[1]
  return(ba.sum)
}
