# WI_with_KurobeDamObservation_1965_2024.R
# save *.RData ####
# WI_with_KurobeDamObservation.RData
# WI_with_KurodbDamObservation_1965_2024_AllForestPlots.RData
# plt6.RData

# 1368 -> 1459 -> 1467
0.55*(1467-1368)/100

z<-kurobe_1965_2024
# (dt<--0.55/100*(plt$alt-1368))
(dt<--0.55/100*(plt$alt-1467))

yr.<-1965:2024
wi..<-c()
for(ii in 1:nrow(plt)){
  wi.<-c()
  for(i in yr.){
    wi.<-c(wi.,wi_calc(z$mean[substr(z$date,1,4)==i]+dt[ii]))
  }
  wi..<-cbind(wi..,wi.)
}
wi...<-data.frame(yr.,wi..)

names(wi...)<-c("year",plt$na)

WI_with_KurobeDamObservation_1965_2024_AllForestPlots<-wi...
WI_with_KurobeDamObservation<-wi...[,c("year","Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")]
rownames(WI_with_KurobeDamObservation)<-WI_with_KurobeDamObservation$year
#save(WI_with_KurobeDamObservation,file="../../data/WI_with_KurobeDamObservation.RData")
#save(WI_with_KurobeDamObservation_1965_2024_AllForestPlots,file="../../data/WI_with_KurodbDamObservation_1965_2024_AllForestPlots.RData")
wi_year<-WI_with_KurobeDamObservation

#save(wi_year,file="../../data/wi_year.RData")

# plt5の修正　wiの修正 -> plt6　####
plt6<-plt5
d<-WI_with_KurobeDamObservation
for (ii in 1:nrow(plt6)){
  pn<-plt5$na[ii]
  plt5[ii,paste0("wi",1:7)]
  i<-match(plt5[ii,paste0("yr",1:7)],d$year)
  plt6[ii,paste0("wi",1:7)]<-d[i,pn]
}
#save(plt6,file="../../data/plt6.RData")

# OK !! ####
