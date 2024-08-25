###　Tateyama_BA_periods changes.R
setwd("./test")
load("立山毎木調査_dd_plt.Rdata")
dbh_col=c("d01","d02","d03","d04","d05","d06")
f_col=c("f01","f02","f03","f04","f05","f06")

#' Title
#'
#' @param d   forest data.
#' @param sp  species name. if default is sp="", that operate for all species
#'
#' @return　vector of sum of basal area for each period(year)
#' @export
#'
#' @examples
#' d<-dd[[1]]
#' BA_calc(d,"スギ")
BA_calc <-function(d,sp=""){　#sp="ブナ"
  i<-if(sp==""){1:nrow(d)}else{d$sp==sp} # 種の指定が""の時全種
  dbh<- d[i,dbh_col]
  dbh[dbh<10]<-0
	f<-d[i,f_col] ;f[is.na(f)]<--999
	live<-f ; live[f>0]<-1 ; live[f<=0]<- 0 
	ba <- pi*(dbh/200)^2
	ba.live <- ba * live
	ba.sum<-colSums(ba,na.rm=TRUE)/plt$Area[ii]*10000
	return(ba.sum)
}

### 活力度を調べていないが生きていた木
(f.<-dd[[4]][,c(f_col)])
table(as.numeric(as.matrix(f.)))  #3 ->9(185)  5 -> 99(123)
sum(is.na(f.))
######
sp.=""

sp.="ブナ"
sp.="スギ"
sp.="オオシラビソ"


BA <- matrix(0,8,6)
for (ii in 1:8){
	d<-dd[[ii]]
	BA[ii,] <- BA_calc(d,sp=sp.)
		   }
	
#### absolute
par(mfrow=c(1,1)) 
 plot(0,type="n" , lty=ii,col=ii,xlim=c(1,6),ylim=c(0,90))
for (ii in 1:8)lines(BA[ii,],type="b",lty=ii,col=ii,pch=ii)

#### ratio
BAr <- BA/BA[,1]
rng<-range(BAr,na.rm=TRUE)

plot(0,type="n" , lty=ii,pch=ii,col=ii,
     xlim=c(1,6) , ylim=c(rng[1]-.1,rng[2]+.1),
     xlab="period",ylab="Basal area ratio",main=sp.)
abline(h=1)


 plr <- which(BA[,1]>0) # plr : plot.recorded
for (ii in plr)lines(BAr[ii,],type="b",lty=ii,pch=ii,col=ii)
legend(1,0.95,plt$na[plr],pch=plr,col=plr,cex=0.7)



#for (ii in 1:8)lines(BAr[ii,],type="b",lty=ii,pch=ii,col=ii)
#legend(1,0.95,plt$na,pch=1:8,col=1:8,cex=0.7)

# species #### 


