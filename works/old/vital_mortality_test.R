# data set #
# setwd("./test")



#' plot relationships vital index and mortality from term 2 to 6
#'
#' @param d
#'
#' @return matrix of mortality
#' @export
#'
#' @examples
#'
#' par(mfrow=c(1,4))
#' vital_mortality(dd2)        # all species ####
#' vital_mortality(dd2,"スギ") #Cryptomeria japonica スギ　####
#' vital_mortality(dd2,"ブナ") # Fagus crenata ブナ　####
#' vital_mortality(dd2,"オオシラビソ") # Abies mariesii オオシラビソ　####
#'

vital_mortality <- function(d.,sp.=""){# sp.="スギ" ;　d.=dd2

  if(sp.==""){d<-d.}else{d<-subset(d.,sp==sp.)}
  d[is.na(d)]<- -999

  mt<-matrix(0,4,5)
  colnames(mt)<-paste0("vital",1:5)
  rownames(mt)<-paste0("term",3:6)

  # 2期の活力度とその後の死亡率
  for (i in 3:6){ #i=6　　1期は欠測があるので除外
    f1<-d[,f_col[2]] ; f2<-d[,f_col[i]]
    t. <- table(f1,f2)
    f1.<- match(1:5,rownames(t.))  #2期に生存していた出現活力度
    f2.<- match(-1:5,colnames(t.))　#i期に倒伏枯死木も含めた出現活力度
    t.<-t.[f1.,f2.]
    t.[,which(is.na(colnames(t.)))]<-0
    t.[which(is.na(rownames(t.))),]<-0

    all.sum  <- rowSums(t.)
    live.sum <- rowSums(t.[,3:7])
    dead.sum <- rowSums(t.[,1:2])
    mt[i-2,] <- dead.sum/ all.sum

  }


  #plot(mt[1,],type="b",ylim=c(0,0.8),xlab="活力度",ylab="死亡率",col=2,lty=1,pch=2)
  if(sp.==""){lbl<-"全種"}else{lbl<-sp.}
  plot(0,type="n",main=lbl,
       xlim=c(1,5),ylim=c(0,1),xlab="活力度",ylab="term2からの死亡率")
  i12<-1:nrow(mt)
  for(i in i12)lines(mt[i,],type="b",col=i,lwd=2,lty=1,pch=i)
  legend(3,.7,rownames(mt),lty=1,pch=i12,col=i12,lwd=2)

  return(mt)

}

