# Fig_Kaminokodaira_Abies_Fagus.R

#' returns basal area of a plot for a spesies
#'
#' @param plotname
#' @param species
#'
#' @return
#' @export
#'
#' @examples
#' plot. <- "Kaminokodaira"
#' sp.1 <- "ooshirabiso"
#' sp.2 <- "buna"
#' ba.sp1 <- BasalArea_1_6(plot.,sp.1)
#' ba.sp2 <- BasalArea_1_6(plot.,sp.2)
#' Year <- ba.sp1$Year
#' rba.sp1 <- 100*ba.sp1[,2]/ba.sp1[1,2]   #relative basal area
#' rba.sp2 <- 100*ba.sp2[,2]/ba.sp2[1,2]   #relative basal area
#' plot(Year,rba.sp1,ylab="Basal area (%)", ylim=c(60,200),
#' type="b",lwd=5,pch=2,col="blue", main="Kaminokodaira (a.s.l. 1450m?@ecotone)")
#' lines(Year,rba.sp2,type="b",col="orange",lwd=5)
#' abline(h=100,col="red",lty=2,lwd=2)
#' legend(2000,180,c("Abies mariesii","Fagus crenata"),lwd=5,pch=c(2,1),col=c("blue","orange"))
BasalArea_1_6 <- function(plotname="Kaminokodaira",species= "ooshirabiso"){
  ii<-which(plt$na==plotname)
  d <- dd3[dd3$plot==ii,]
  yrc<-match(paste0("yr",1:6),names(plt))
  dbhc<-match(paste0("d0",1:6),names(d))
  ba <- pi*(d[,dbhc]/200)^2
  sp_ = species
  i.sp <- d$sp == sp_
  ba_ <- ba[i.sp,]
  BasalAera <- as.vector(colSums(ba_,na.rm=T))
  Year <-as.numeric(plt[ii,yrc])
  return(data.frame(Year,BasalAera))
}

