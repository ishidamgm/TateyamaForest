#### 枯死木集計001.R
#C:\Users\ishid\Dropbox\00D\00\tateyama\Tateyama_forest\_Tateyama_forest.docx
# clm
vital.clm <- c("f02","f03","f04","f05","f06")
  dbh.clm <- c("d02","d03","d04","d05","d06")
  yr.clm <- c("yr2","yr3","yr4","yr5","yr6")

getwd()
setwd("./test")  

load("立山毎木調査_dd_dd2_plt.Rdata")  ### dd2を読まなければならない
# 
# survey years ####

yr <- plt[,c("yr1","yr2","yr3","yr4","yr5","yr6")]
rownames(yr) <- plt$na
(yr2 <- data.frame(yr,years=yr$yr6-yr$yr1+1))
summary(yr) #1998-2019
# vital index ####
data.frame(names(dd2))
vi <- dd2[,c(10:15)]
(dn <- nrow(vi))  #3642
edit(vi)
# vital index

vi.vector <- as.vector(as.matrix(vi))
table(vi.vector )
#ブナ平と松尾峠の初回は活力度の計測なし

sum(is.na(vi.vector))   #3853
sum(table(vi.vector ))  #17999
(17999+3853)/6 #3642
sum(is.na(dd2$f06)) # 期末にNAの個体数が0であること確認
sum(dd2$f06>0)/dn # 期末に生存している個体 3099 枯死した記録がない個体　0.8509061
# 樹種別に検討する

life_death  <- table(dd2$sp,dd2$f06<=0)
m <- matrix(as.vector(life_death),ncol=2)
life_death <- data.frame(sp=rownames(life_death),life=m[,1],death=m[,2],mortality=m[,2]/(m[,1]+m[,2]))
life_death

# オオシラビソ かつ　期首に計測
i<-dd2$sp=="オオシラビソ" & !is.na(dd2$d01) & dd2$d01>=10
#i<-dd2$sp=="ブナ" & !is.na(dd2$d01) & dd2$d01>=10
(n. <- sum(i)) #解析対象本数
dd2. <- dd2[i,]
(plt. <- unique(dd2$plot[i]))  #出現プロット
pltn<-length(plt.)
# 枯死個体割合　解析対象　1期にdbhに生存していた直径10cm以上の個体
par(mfrow=c(2,2))
live_die<-c()
for(ii in 1:pltn){#ii<-1
  jj<-plt.[ii]
  plot.<-plt$na[jj]
  d <- dd2.[dd2.$plot==jj,]
  i.live<-d$f06>0
  mortality<-1-sum(i.live)/nrow(d) #
  mortality.<-round(mortality*100,2)
  paste(plot.,yr2$years[jj],"年間で全本数の",round(mortality*100,2),"%が枯死した。")
  
  dbh.cls<-c(seq(10,80,10),400)
  dbh.freq01<-table(cut(d$d01,dbh.cls))
  #barplot(dbh.freq01) #期首の直径分布
  dbh.freq01.die<-table(cut(d$d01[d$f06<=0],dbh.cls))
  dbh.freq01.live_die<-rbind(dbh.freq01-dbh.freq01.die,dbh.freq01.die)
  barplot(dbh.freq01.live_die,xlab="胸高直径(cm)",ylab="本数",
          main=paste0(plt$na[jj],":",plt$yr1[jj],"-",plt$yr6[jj],"死亡率:",mortality.,"%"),
          col=c("white","black"))
  legend(6,max(dbh.freq01),c("生存木","枯死木"),pch=c("□","■"))
  live_die<-c(live_die,list(list(dbh=d$d01,live=i.live,mortality=mortality.,dbh.freq01.live_die=dbh.freq01.live_die)))
}

names(live_die)<-plt$nm[plt.]

# 積算
for(ii in 1:pltn){
  
  d<-live_die[[ii]]$dbh.freq01.live_die
  mortality.<-live_die[[ii]]$mortality
  dbh.freq01.live_die_cumsum<-rbind(cumsum(d[1,]),cumsum(d[2,]))
  barplot(dbh.freq01.live_die_cumsum,xlab="胸高直径(cm)",ylab="積算本数",
          main=paste0(plt$na[jj],":",plt$yr1[jj],"-",plt$yr6[jj],"死亡率:",mortality.,"%"),
          col=c("white","black"))
}

#期首でnaだったものは除く

head(dd2)









# all individuals ####
all.trees<-dd2$sp
length(all.trees)            ### Total number of trees 3642
all_trees <- data.frame(table(all.trees)) ### Total number of trees for each tree species
# write.csv(all_trees,file="all_trees.csv")
###
vital <- c()
for(ii in 1:length(dd)){
  all.trees<-c(all.trees,dd[[ii,vital.clm]])
}


##　調査地松尾峠
d <- dd[["Matsuotoge"]]
names(d)


Abies <- d$sp=="オオシラビソ"
sum(Abies)  # 全本数　284本

#### 全期間記録していた個体　 174本
sum(rowSums(d[i,clm]>0)==5,na.rm=T)

####　期間中に進階した個体 42本　NAあり
sum(is.na(d[i,"f02"]))

####　期首に立ち枯れしていた木 0本
sum(d[i,"f02"]==0)


####　期末に立ち枯れしていた木 10本
sum(d[i,"f06"]==0)

####　期末に倒れていた木 61本

sum(d[i,"f06"]==-1)


#### 倒木記録回数　
hist(rowSums(d[i,clm]==-1))
d[331,clm]

#### 立ち枯れ記録回数　
hist(rowSums(d[i,clm]==0))

#### 初回計測した後立ち枯れを経ず倒木した木 12本

sum(rowSums(d[i,clm]==-1)==5,na.rm=T)
