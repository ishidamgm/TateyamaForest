###　Tateyama_BA_periods changes.R
#


#' Calculates basal areas of each period
#'
#' omit tree data with is.na(dbh) and dbh < 10cm
#'
#' @param d   forest data.
#' @param sp  Roman species name. if default is sp="", that operate for all species
#'
#' @param dbh.min  a minimum value of DBH for calculation (default = 10)
#'
#' @param f.min  a minimum value of vital index for calculation  (default = 1)
#'
#'
#' @return　vector of sum of basal area for each period(year)
#' @export
#'
#' @examples
#' plt
#' plot.<-"Matsuotoge"
#' d<-subset(dd3,plot==plot.) #d<-subset(dd3.,pn==6) #
#' BA_calc(d,sp="オオシラビソ")
#' (ba.<-BA_calc(d,""))
#' ba./ba.[1]
#' plot.<-"Mimatsu"
#' d<-subset(dd3,plot==plot.)
#' (ba.<-BA_calc(d,"",dbh.min=0))
#'  (ba.<-BA_calc(d,"",dbh.min=10))
#' plot.<-"Kagamiishi"
#' d<-subset(dd3,plot==plot.)
#' plot(ba.<-BA_calc(d,"",dbh.min=0),type="b",ylim=c(0.4,1),main=plot.)
#' lines(ba..<-BA_calc(d,"",dbh.min=10),col="red",type="b")
#' legend(1.5,1.15,c("dbh>0cm","dbh>10cm"),col=c("black","red"),lty=c(1,1))
#'
#' plot(ba./ba.[1],type="b",ylim=c(0.9,1.2),main=plot.)
#' lines(ba../ba..[1],type="b",col="red")
#' legend(1.5,1.15,c("dbh>0cm","dbh>10cm"),col=c("black","red"),lty=c(1,1))
#'
BA_calc <-function(d,sp="",dbh.min=10,f.min=1){　#sp="buna"
  i<-if(sp==""){1:nrow(d)}else{d$sp==sp} # 種の指定が""の時全種
  f<-d[i,clm_f]
  dbh<- d[i,clm_dbh]
  dbh[dbh<dbh.min | f<f.min ]<-0
	ba <- pi*(dbh/200)^2
	ba.sum<-colSums(ba,na.rm=TRUE)
	return(ba.sum)
}


#' Calculates basal areas of each period
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
#' plot.<-"Matsuotoge"
#' d<-subset(dd3,plot==plot.) #d<-subset(dd3.,pn==6) #
#' BA_calc(d,"オオシラビソ")
#' (ba.<-BA_calc(d,""))
#' ba./ba.[1]
BA_calc.old <-function(d,sp=""){　#sp="buna"
  i<-if(sp==""){1:nrow(d)}else{d$sp==sp} # 種の指定が""の時全種
  f<-d[i,clm_f]
  dbh<- d[i,clm_dbh]
  dbh[dbh<10 | f<1 ]<-0
  ba <- pi*(dbh/200)^2
  ba.sum<-colSums(ba,na.rm=TRUE)
  return(ba.sum)
}

#' This makes Basal area matrix for 6 periods of tree species or All species
#'
#' @param d   data frame of forest stand trees
#' @param sp
#'
#' @param dbh.min  a minimum value of DBH for calculation (default = 10)
#'
#' @param f.min  a minimum value of vital index for calculation  (default = 1)
#'
#' @return
#' @export
#'
#' @examples
#' plt
#' plot.<-"Kaminokodaira"
#' d<-subset(dd3,plot==plot.)
#'
#' # Vital check ####
#' (f.<-d[,c(clm_f)])
#' table(as.numeric(as.matrix(f.)))
#' sum(is.na(f.))
#'
#'
#' # all tree species ####
#' sp.<- ""
#' (BA <- BA_matrix(sp.))
#'  (BA0 <- BA_matrix(sp.,dbh.min=0))
#' # Kagamiishi trees with more 1.3m height, others trees with more 10cm dbh
#' BA[7,]<-BA0[7,]
#'
#' # absolute ###
#' par(mfrow=c(1,1))
#' plot(0,type="n" ,xlim=c(1,6),ylim=c(0,90),
#' xlab="Period",ylab="Basal Area (m*m)")
#' for (ii in 1:8)lines(BA[ii,],type="b",lty=leg$lty[ii],col=leg$col[ii],pch=leg$pch[ii])
#'
#' # ratio ####
#' (BAr <- BA/BA[,1])
#' rng<-range(BAr,na.rm=TRUE)
#' plot(0,type="n" , lty=ii,pch=ii,col=ii,
#'      xlim=c(0.25,6) , ylim=c(rng[1]-.02,rng[2]+.02),
#'      xlab="period",ylab="Basal area ratio",main=sp.)
#' abline(h=1)
#' plr <- which(BA[,1]>0) # plr : plot.recorded
#' for (ii in plr)lines(BAr[ii,],type="b",
#'                      lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
#' legend(1.2,1.2,leg$n,pch=leg$pch,col=leg$col,lty=leg$lty,cex=0.7)
#'
#'
#'
#' # "ooshirabiso" ####
#' sp.<- "オオシラビソ"
#' (BA <- BA_matrix(sp.))
#' (BA0 <- BA_matrix(sp.,dbh.min=0))
#' Kagamiishi trees with more 1.3m height, others trees with more 10cm dbh
#' BA[7,]<-BA0[7,]
#' # absolute ###
#' par(mfrow=c(1,1))
#' plot(0,type="n" ,xlim=c(1,6),ylim=c(0,90),
#' xlab="Period",ylab="Basal Area (m*m)")
#' for (ii in 1:8)lines(BA[ii,],type="b",lty=leg$lty[ii],col=leg$col[ii],pch=leg$pch[ii])
#'
#' # ratio ####
#' (BAr <- BA/BA[,1])
#' rng<-range(BAr,na.rm=TRUE)
#' plot(0,type="n" , lty=ii,pch=ii,col=ii,
#'      xlim=c(1,6) , ylim=c(rng[1]-.1,rng[2]+.1),
#'      xlab="period",ylab="Basal area ratio",main=sp.)
#' abline(h=1)
#' plr <- which(BA[,1]>0) # plr : plot.recorded
#' for (ii in plr)lines(BAr[ii,],type="b",
#'                      lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
#'  i<-4:7
#' legend(1,0.85,leg$n[i],pch=leg$pch[i],col=leg$col[i],lty=leg$lty[i],cex=0.7)
#'
#'
BA_matrix <- function(sp="オオシラビソ",dbh.min=10,f.min=1){
  BA <- matrix(0,8,6)
  for (ii in 1:8){
    d<-subset(dd3,pn==ii)
    BA[ii,] <- BA_calc(d,sp,dbh.min=dbh.min,f.min=f.min)
  }
  return(BA)
}



BA_matrix("オオシラビソ",dbh.min=0)
BA_matrix()
dd3

# BA_matrix.old <- function(sp="ooshirabiso"){
#   BA <- matrix(0,8,6)
#   for (ii in 1:8){
#     d<-subset(dd3,pn==ii)
#     BA[ii,] <- BA_calc(d,sp)
#   }
#   return(BA)
# }



BAr_fig<-function(BA,plot,sp,xlim,ylim,xleg,yleg){
  BA <- BA_matrix(d,sp)
  rng<-range(BAr,na.rm=TRUE)
  plot(0,type="n" , lty=ii,pch=ii,col=ii,
       xlim=c(1,6) , ylim=c(rng[1]-.1,rng[2]+.1),
       xlab="period",ylab="Basal area ratio",main=sp.)
  abline(h=1)
}

# ratio ####
(BAr <- BA/BA[,1])
rng<-range(BAr,na.rm=TRUE)
plot(0,type="n" , lty=ii,pch=ii,col=ii,
     xlim=c(1,6) , ylim=c(rng[1]-.1,rng[2]+.1),
     xlab="period",ylab="Basal area ratio",main=sp.)
abline(h=1)


 plr <- which(BA[,1]>0) # plr : plot.recorded
for (ii in plr)lines(BAr[ii,],type="b",
                     lty=leg$lty[ii],pch=leg$pch[ii],col=leg$col[ii])
 legend(1,0.95,leg$n,pch=leg$pch,col=leg$col,lty=leg$lty,cex=0.7)












