# dbh_hist.R ####



#' histgram of dbh including dead standing trees
#'
#' @param plotname
#' @param species
#' @param term
#'
#' @return
#' @export
#'
#' @examples
#' par(mfrow=c(2,3))
#' for(i in 1:6){
#' dbh_hist("Kaminokodaira","ooshirabiso", i,ylim=c(0,13))
#' }
#'
dbh_hist <- function(plotname="Kaminokodaira",species="ooshirabiso", term=1,...){
  ii<-which(plt$na==plotname)
  d <- dd3[dd3$plot==ii,]
  sp <- d$sp
  yrc<-match(paste0("yr",1:6),names(plt))
  dbhc<-match(paste0("d0",1:6),names(d))
  fc<-match(paste0("f0",1:6),names(d))
  dbh. <- d[,dbhc[term]]
  f.   <- d[,fc[term]]

  Year <- plt[ii,yrc[term]]

  i <- sp==species & !is.na(dbh.) & dbh.>=10
  j <- f.>0

h.all  <- hist(dbh. [i],col="black",
               xlab="DBH (cm)", main=paste(plotname,Year, species,sep="_"),...)
h.dead <- hist(dbh. [i & f.>0],col="white",add=T)

}
