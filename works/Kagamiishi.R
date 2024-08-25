# Kagamiishi.R



par(mfrow=c(2,3))
for(i in 1:6){
  dbh_hist2("Kagamiishi","ooshirabiso", 0,i,ylim=c(0,100))
}


dbh_hist2<-function (plotname = "Kagamiishi", species = "ooshirabiso", minDBH=0,
          term = 1, ...) {
  ii<-which(plt$na==plotname)
  d <- dd3[dd3$plot == plotname, ]
  sp <- d$sp
  yrc <- match(paste0("yr", 1:6), names(plt))
  dbhc <- match(paste0("d0", 1:6), names(d))
  fc <- match(paste0("f0", 1:6), names(d))
  dbh. <- d[, dbhc[term]]
  f. <- d[, fc[term]]
  Year <- plt[ii, yrc[term]]
  i <- sp == species & !is.na(dbh.) & dbh. >= minDBH
  j <- f. > 0

  cls<-seq(0,30,5)

  h.all <- hist(dbh.[i],  breaks=cls,
                col = "black", xlab = "DBH (cm)",
                main = paste(plotname, Year, species, sep = "_"), ...)

  h.dead <- hist(dbh.[i & f. > 0], breaks=cls,
                 col = "white", add = T)
}






#

d. <- dd3[dd3$plot =="Kagamiishi", ]
nrow(d.)

#' plt
#' plot.<-"Kagamiishi"
#' d<-subset(dd3,plot==plot.)
#' BA_calc(d,"ooshirabiso")
#' BA_calc(d,"")
#'
BA_calc <-function(d,sp=""){　#sp="ooshirabiso"
  i<-if(sp==""){1:nrow(d)}else{d$sp==sp} # 種の指定が""の時全種
  dbh<- d[i,clm_dbh]
  #dbh[dbh<10]<-0
  f<-d[i,clm_f] ;f[is.na(f)]<--999
  live<-f ; live[f>0]<-1 ; live[f<=0]<- 0
  ba <- pi*(dbh/200)^2
  ba.live <- ba * live
  ba.sum<-colSums(ba,na.rm=TRUE)
  return(ba.sum)
}



#### dd3 の直径データ
for (ii in 1:length(plt$n)){
  d<-subset(dd3,plot==plt$n[ii])
  print(paste(plt$n[ii],range(d$d01,na.rm = T)))
}
