% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fig2024.R
\name{dbh_hist_term1to7}
\alias{dbh_hist_term1to7}
\title{histgram of dbh including dead standing trees
in 2000 (term 1) at Kaminokodaira}
\usage{
dbh_hist_term1to7(
  plotname = "Kaminokodaira",
  main = "",
  species = "オオシラビソ",
  dbh_min = 10,
  breaks = seq(10, 50, 10),
  legend = TRUE,
  ...
)
}
\arguments{
\item{term}{}
}
\description{
histgram of dbh including dead standing trees
in 2000 (term 1) at Kaminokodaira
}
\examples{
par(mfrow=c(1,3))
dbh_hist_term1to7(plotname="Kaminokodaira",main="Ecotone plot",species="オオシラビソ",legend=F)
dbh_hist_term1to7(plotname="Matsuotoge",main="Subarctic plot",species="オオシラビソ",legend=F)
dbh_hist_term1to7(plotname="Kagamiishi",main="Timberline plot",species="オオシラビソ",legend=F,breaks=seq(0,30,5))
legend(1.5,53,legend=c("Dead Standing (fallen)","Dead Standing ","Living"),
 fill=c("black","black","white"),density=c(NA,20,NA))

}
