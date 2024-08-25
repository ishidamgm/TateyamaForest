#### 枯死木集計001.R
names(dd)

##　調査地松尾峠
d <- dd[["Matsuotoge"]]
names(d)
clm <- c("f02","f03","f04","f05","f06")

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