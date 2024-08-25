###　Tateyama_DiametricSpecies_2020.R

datdir <- "./Tateyama_forest"
setwd(datdir)
(f<-dir())
plt <- read.csv("plot profile.csv" ,skip=1)
head(plt)

#### データファイル読み込み

(plt$file=paste0(plt$na,c(2016,2015,2017,2018,2016,2019,2017,2019),".csv"))
names(plt)

dd<-c(); for(i in 1:nrow(plt))dd<-c(dd,list( read.csv(plt$file[i])))

edit(dd[[1]])
##### diametric-species 


 diam_sp1<-function(sp,dbh){
  Species <- unique(sp)
  SpeciesNumber <- length(Species)
  cls <- seq(0,110,5)					# 5cm刻みの直径階を作成
  ds <- table(sp,cut(dbh,cls))				#　各樹種の直径階別本数の一覧表)
  DiametrinSpecies <- t(ds)
  DiametrinSpeciesNumber <- length(ds[ds!=0])
  df<-data.frame(SpeciesNumber ,DiametrinSpeciesNumber)	
  return(list(Species, DiametrinSpecies,df))
				}

diam_sp<-function(sp,dbh){

  d<-diam_sp1(sp,dbh)
  Species<-d$Species ; DiametrinSpecies<-d$DiametrinSpecies ;df <-d$df
  n <- length(sp)
  NN<-c()
  for (ii in 1:n){
  i<-sample(n,ii)
  NN<-rbind(NN,diam_sp1(sp[i],dbh[i])[[3]])
							
		    }

    return(list(sp=Species, DiametrinSpecies=DiametrinSpecies,N=df,NN=NN))
}


#z<-diam_sp(sp,dbh)$NN
#plot(z[,2],type="l")
#lines(z[,1],col="blue")

##### save(dd,file="dd.RData")

dset<-function(d){
#ii<-1
	d<-dd[[ii]]
	d_<-na.omit(data.frame(sp=d$sp,dbh=d[,33]))
	(dsp<-diam_sp(d_$sp,d_$dbh))
	return(dsp$NN)
}

NN<-dset(dd[[1]])
plot(NN[,2],type="l",col=1,xlim=c(0,700),ylim=c(0,90),
xlab="Individuals",ylab="Diametric Species",
 main="Diversity of Diametric Species (Gonzalo & Timo, 2020)")


for (ii in 2:length(dd)){
  d<-dd[[ii]]
  j<-which(substr(names(d),1,1)=="D")[1]
  d_<-na.omit(data.frame(sp=d$sp,dbh=d[,j]))
  dsp<-diam_sp(d_$sp,d_$dbh)
  NN<-dsp$NN
  lines(NN[,2],type="l",col=ii)
}





legend(400,50,paste0(plt$na,"_",plt$alt,"m"),lty=1,col=1:nrow(plt))








