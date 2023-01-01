# TateyamaForest_dd3_plt_RData.R

if(0){
  load("立山毎木調査_dd_dd2_plt.Rdata")  ### dd2を読まなければならない
  clm_f <- c("f02","f03","f04","f05","f06")
  clm_dbh <- c("d02","d03","d04","d05","d06")
  clm_yr <- c("yr2","yr3","yr4","yr5","yr6")
  d<-dd2
  #save(vital.clm,dbh.clm,yr.clm,file="../data/clm.RData")
}


dd3<-dd2
dd3$sp<-stringi::stri_trans_general(dd3$sp, "Any-latn")
edit(dd3)
dir("./data")
#save(dd3,plt,file="TateyamaForest_dd3_plt.RData")



