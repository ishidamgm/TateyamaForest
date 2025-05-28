# PopulationGrowth_Condit.R



#' Population growth (Condit  et al.)
#' save(PopulationGrowth,file="data/PopulationGrowth.RData")
#'
#'
#' @param plot.   default = "Kaminokodaira".
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#'  Population_growth("Kaminokodaira")
#'  str(Population_growth("Kaminokodaira"))
#'   Population_growth("Kagamiishi")
#'   Population_growth("Bunazaka")
#' l<-lapply(plt2$na,Population_growth)
#' names(l)<-plt2$na
#' (PopulationGrowth <- l)
#'
#' # save(PopulationGrowth ,file="data/PopulationGrowth.RData")
Population_growth <- function(plot.="Kaminokodaira"){
  ii<-match(plot.,plt2$na)
  t<- plt2$yr7[ii]-plt2$yr1[ii]   # 調査期間　(年)

  l. <- subset(dd5,plot==plot.)
  sp.<- unique(l.$sp)

  pg <- data.frame(
    sp = as.character(NA),  # 文字列 (string)
    n0 = as.integer(NA),    # 整数 (integer)
    n1 = as.integer(NA),
    nS = as.integer(NA),
    t  = as.integer(NA),
    p  = as.numeric(NA),    # 実数 (real)
    m  = as.numeric(NA),
    r  = as.numeric(NA)
  )
  pg<-pg[-1,]
  for(i in 1:length(sp.)){
    sp.. <- sp.[i]
    l<-subset(dd5,plot==plot. & sp==sp..)

    # plot.="Kagamiishi"

    if(plot.!="Kagamiishi"){
      n0 <- sum(l$d01>=10 & l$f01>0,na.rm=T)
      n1 <- sum(l$d07>=10 & l$f07>0,na.rm=T)
      nS <- sum(l$f01>0 & l$f07>0,na.rm=T)
    } else {
      n0 <- sum(l$f01>0,na.rm=T)
      n1 <- sum(l$f07>0,na.rm=T)
      nS <- sum(l$f01>0 & l$f07>0,na.rm=T)
    }

  # c(n0,n1,nS)

    p <- log(n1/n0)/t *100
    m <- log(n0/nS)/t *100
    r <- log(n1/nS)/t *100
    #pg[i,]<-c(sp..,n0,n1,nS,p,m,r)
    pg <- rbind(pg, data.frame(sp = sp.., n0 = n0, n1 = n1, nS = nS, t=t, p = p, m = m, r = r))
  }


  return(pg)
}


#' DemographicRates  (Population growth : Condit  et al.)
#'
#' @param n0   Population size at first survey
#' @param n1   Population size at latest survey
#' @param nS   Population size of survivors
#' @param t    census intervals (years)
#'
#' @return
#' @export
#'
#' @examples
#' DemographicRates(n0=100, n1=150, nS=80,t=1)
#' DemographicRates(n0=100, n1=100, nS=100,t=1)
#' DemographicRates(n0=100, n1=110, nS=100,t=1)
#'
DemographicRates <- function(n0=100, n1=150, nS=80,t=10){
  p <- log(n1/n0)/t *100
  m <- log(n0/nS)/t *100
  r <- log(n1/nS)/t *100
  return(data.frame(n0, n1, nS,t,p,m,r))
}




# Table ####
spl<-SpeciesList5
l<-PopulationGrowth
l..<-c()
for(ii in 1:length(l)){
  l.<-l[[ii]]
  zone <- spl$zone[match(l.$sp,spl$spj)]
  l..<-rbind(l..,data.frame(plot=names(l)[ii],zone,l.))
}

PopulationGrowth_df <-l..
# save(PopulationGrowth_df ,file="data/PopulationGrowth_df.RData")
# write.csv(PopulationGrowth_df,file="data_raw/PopulationGrowth_df.csv")


l<-PopulationGrowth_df

plot.zone <- data.frame(
  plot= c("Bunazaka","Kaminokodaira","Kaminokodaira","Kaminokodaira","Matsuotoge","Kagamiishi"),
  zone= c("Temperate","Temperate","Ecotone","Subarctic","Subarctic","Subarctic")
)

plot.zone

l..<-c()
for(ii in 1:nrow(plot.zone)){
  l.<-subset(l,plot== plot.zone[ii,1] & zone==plot.zone[ii,2])
  nt<-c(sum(l.$n0),sum(l.$n1),sum(l.$nS),l.$t[1])
  l..<-rbind(l..,DemographicRates(nt[1],nt[2],nt[3],nt[4]))
}


(PopulationGrowth_zone_df<-data.frame(plot.zone,l..))

# save(PopulationGrowth_zone_df ,file="data/PopulationGrowth_zone_df.RData")

# write.csv(PopulationGrowth_zone_df ,file="data_raw/PopulationGrowth_zone_df.csv")

# 各プロット全種DemographicRates ####
l<-PopulationGrowth_zone_df
l$sp
l<-data.frame(cbind(tapply(l$n0,l$plot,sum),tapply(l$n1,l$plot,sum),tapply(l$nS,l$plot,sum)  ) ,c(22,24,24,23) )
names(l)<-c("n0","n1","nS","t")
unique(l$plot)

DemographicRates_all <- DemographicRates(l$n0,l$n1,l$nS,l$t)
rownames(DemographicRates_all)<-rownames(l)
DemographicRates_all

# 【確認・修正】PopulationGrowthの表　集計　があっていないのではないか?　2025/5/8 ####
l<-PopulationGrowth$Kaminokodaira
colSums(l[,c("n0")])
c(sum(l$n0),sum(l$n1),sum(l$nS))
DemographicRates(sum(l$n0),sum(l$n1),sum(l$nS),24)　#全体 あっている
DemographicRates(34,21,19,24) #subarctic あっている
PopulationGrowth_zone_df
spl<-SpeciesList5
z<-spl$zone[match(l$sp,spl$spj)]
l[z=="Subarctic",]
# > l[z=="Subarctic",]
# sp n0 n1 nS  t       p        m         r
# 9  オオシラビソ 30 17 15 24 -2.3666 2.888113 0.5215131
# 11     コメツガ  2  2  2 24  0.0000 0.000000 0.0000000
# 12   ダケカンバ  2  2  2 24  0.0000 0.000000 0.0000000

# >>> コメツガがあった　#####
# 案1　others とする　
# 案2　脚注に10本以上を記録した樹種のみを示す。全体，各要素の集計は全記録種で計算した。

# Only species with 10 or more recorded individuals are shown in this table.
# However, demographic rates for the total and for each element were
# calculated based on all recorded species, including those not listed.

# Kagamiishも不一致だ
PopulationGrowth$Kagamiish
PopulationGrowth_zone_df
#　集計はあっているが　オオシラビの値　表とPopulationGrowth$Kagamiish　違う
#　手作業でやったので間違った?
#　SpeciesList4.RData　中島さんのコードを拝借した
#　PopulationGrowthは2024の報告書の中島さんの値と一致している
PopulationGrowth
SpecieList5
#　>>>　対応　手作業でTable　4　の鏡石の集計部分を修正する
# SpeciesList4 を改める案


# 確認 ####
PopulationGrowth_df
# ブナ坂のヤマモミジの初期本数よりも全期生存木本数が上回っている

subset(PopulationGrowth_df,sp=="ヤマモミジ" & plot=="Bunazaka")
.<-subset(dd4,sp=="ヤマモミジ" & plot=="Bunazaka")
.

#　10cm　以上　d01>10 -> d01>=10で直る


# ミネカエデは低木
(sp_dd4 <- unique(dd4$sp))
SpeciesList3
SpeciesList
data(package="TateyamaForest")

unique(SpeciesList$form)
(sp_tree <- subset(SpeciesList,form!="bs")$spj)
sp_tree[is.element(sp_dd4,sp_tree )]

SpeciesList4$spj
SpeciesList

# dd5.RData ####
# 樹種のリストが不整合  ====>　解決　dd5　SpeciesList5   高木　24種　に限定　
#　SpeciesList$form ナナカマド(小高木)とミネカエデ(低木)がともにbs
# マルバマンサクを除外しているが、ナナカマドを除外するのは妥当か?
#　オノエヤナギは?
unique(dd4$plot)
dd4$sp=="オノエヤナギ"
dd5<-dd4[is.element(dd4$plot,plt2$na) & is.element(dd4$sp,sp_tree),]
c(nrow(dd4),nrow(dd5))
(sp_dd5 <- unique(dd5$sp))
tapply(dd5$plot, dd5$plot, length)

# save(dd5 ,file="data/dd5.RData")
# write.csv(dd5,file="data_raw/dd5.csv")

SpeciesList5 <- SpeciesList[is.element(SpeciesList$spj,sp_dd5),]
rownames(SpeciesList5)<-1:nrow(SpeciesList5)
SpeciesList5

# save(SpeciesList5 ,file="data/SpeciesList5.RData")
# write.csv(SpeciesList5,file="data_raw/SpeciesList5.csv")


#

