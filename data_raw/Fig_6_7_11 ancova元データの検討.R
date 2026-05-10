
#　Fig_6_7_11 ancova元データの検討.R ####

# FIGURE 6 | Changes in basal area in the Ecotone plot over time. ####

Fig_yr_ba_kaminokodaira_zone_2024()
Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024()
Fig_yr_ba_Kaminoko_JVS2()


##　生データ ####
sp_ba[["Kaminokodaira"]]["オオシラビソ",]　
##　対林分 ####
sp_ba[["Kaminokodaira"]]["オオシラビソ",]/colSums( sp_ba[["Kaminokodaira"]])
# 2000       2004       2007       2010       2013       2018       2024
# 0.04941790 0.04757411 0.04181103 0.04173711 0.04184712 0.03898118 0.02600110
#　sp_ba_ratioは　対林分 ####
(A<-sp_ba_ratio[["Kaminokodaira"]]["オオシラビソ",])
# 2000       2004       2007       2010       2013       2018       2024
# 0.04941790 0.04757411 0.04181103 0.04173711 0.04184712 0.03898118 0.02600110
## 初期値1に基準化 ####
A/A[1]
# 2000      2004      2007      2010      2013      2018      2024
# 1.0000000 0.9626897 0.8460705 0.8445747 0.8468009 0.7888069 0.5261474

# FIGURE 7 | Relationships between the warmth index (WI) and the basal area ratio at the Ecotone plot. ####
Fig_wi_ba_cor_ancova_JVS2()
.data_Fig_yr_ba_kaminokodaira_zone_2024
.data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024
## Fig.6と同じ　####

# FIGURE 11 | Relationships between the warmth index (WI) and (a) basal area ratio and (b) cumulative mortality ratio of _Abies mariesii_ in the Timberline, Subarctic, and Ecotone plots. ####
Fig_Abies_wi_ba_mortality()
(.<-TemperatureWIAbies_Population_BA_Mortality)
.$BAratio



# Basal area (種内)　報告書
a<-sp_ba$Kaminokodaira["オオシラビソ",]
a/a[1]　　
plot(a/a[1],type="b"　)
#'      2000      2004      2007      2010      2013      2018      2024
#'  1.0000000 1.0023957 0.9006119 0.9314482 0.9520498 0.9358946 0.6415239
#'
#'
#'
plot(a)
plot(rba.$Kaminokodaira)  #自らの1期を1とした
TemperatureWIAbies_Population_BA_Mortality　#:rba.を参照した

#' これをancovaに使った　
Fig_Abies_wi_ba_mortality()

#' もう一つのancova　
Fig_wi_ba_cor_ancova_JVS2()

#'  .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024を使っている
#'  Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024()
#'  sp_ba_ratio$Kaminokodaira["オオシラビソ",]を使っている
#'        2000       2004       2007       2010       2013       2018       2024
#'　0.04941790 0.04757411 0.04181103 0.04173711 0.04184712 0.03898118 0.02600110



a/a[1]　　　#中島さんと共通----これとは異なる

#'      2000      2004      2007      2010      2013      2018      2024
#'  1.0000000 1.0023957 0.9006119 0.9314482 0.9520498 0.9358946 0.6415239
#'


#'　
#'　sp_ba_ratioは林分全体で割っているのでは?
#'　 yr_sp_ba_site()のExamplesで作っていた
#'　#'(sp_ba_ratio<-sapply(sp_ba,function(x)t(t(x)/rowSums(t(x)))))
#'
#'# save(sp_ba_ratio,file="data/sp_ba_ratio.RData")
#'# save(sp_ba_ratio_StandigDead,file="data/sp_ba_ratio_StandigDead.RData")
#'
#'　sapply(sp_ba,function(x)t(t(x)/rowSums(t(x)))))
#'　t(sapply(sp_ba,function(x)t(t(x)/rowSums(t(x))))))
#'x<-sp_ba$Kaminokodaira
#'t(t(x)/rowSums(t(x)))
#'

sapply(sp_ba,function(x)t(t(x)/rowSums(t(x))))
sapply(sp_ba,function(x)t(t(x)/t(x)[,1]))
t(sp_ba[["Kaminokodaira"]])[1,]
x<-sp_ba[["Kaminokodaira"]]

#  sp_ba_ratioの確認 ####
sp_ba[["Kaminokodaira"]]["オオシラビソ",]
# 2000     2004     2007     2010     2013     2018     2024
# 2.450286 2.456156 2.206757 2.282314 2.332794 2.293209 1.571917




sp_ba[["Kaminokodaira"]]["オオシラビソ",]

t(t(x)/rowSums(t(x)))

t(t(x)/rowSums(t(x)))
t(x)/t(x)[1,]
x<-matrix(1:9,3)
x/x[,1]
x[,"オオシラビソ"]
z<-x/colSums(x)
z["オオシラビソ",]/0.04480884
z["オオシラビソ",]/colSums(x)/0.0009037144
#1.0000000 0.9974148 0.8961368 0.9314481 0.7659431 0.7332213 0.5161188

#'　
#'　
#' 20260501時点で方法の記載と食い違いがある
#'
