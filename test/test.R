#' Title
#'
#' @param x
#'
#' @return x^2
#' @export
#'
#' @examples
#' plot(f(1:10))
f <- function(x){
  x^2
}

#' Title
#'
#' @param x
#'
#' @return x^2
#' @export
#'
#' @examples
#' plot(f3(1:10))
f3 <- function(x){
  x^3
}

#' test script
#'
#' @param x
#'
#' @return x^2
#' @export
#'
#' @examples
#' txt <- 'zz'
#'jtxt(txt)
j_txt <- function(x){
  paste0(x,1:5)
}



#' Title
#'
#' @param plotname
#' @param species
#'
#' @return
#' @export
#'
#' @examples
BasalArea16___ <- function(plotname,species){
  d <- dd2[dd2$plot==ii,]

  yrc<-match(paste0("yr",1:6),names(plt))

  dbhc<-match(paste0("d0",1:6),names(d))
  ba <- pi*(d[,dbhc]/200)^2
  sp_ = species
  i.sp <- d$sp == sp_
  ba_ <- ba[i.sp,]
  BasaAera <- as.vector(colSums(ba_,na.rm=T))
  Year <-as.numeric(plt[ii,yrc])
  return(data.frame(Year,BasaAera))
}
