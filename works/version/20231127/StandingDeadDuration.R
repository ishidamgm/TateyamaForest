#　StandingDeadDuration.R
#
#
# 調査期間中に枯死した木はどのくらいあったか?
#   立ち枯れていた時間はどのくらいか?
#   倒伏するまでの時間はどのくらいか?
#   木は枯れてからどのくらいの時間たっているのか?
#   どうすればわかるか?
#   活力度の利用
# 　調査年の利用
# 　1以上から突然-1になった場合　0～前回調査年までの時間
# 　　　前回の調査直後から今回の調査直前
# 　0から-1になった場合
# 　　　1以上の調査年から(最大)、-1を記録した年


library(tidyverse)
library(TateyamaForest)
dd3
dn <- nrow(dd3)
paste("計測総本数は",dn, "本であった。")
paste("そのうち枯死したりは",dn, "本であった。")

names(dd3)
table(dd3$sp)
d<-subset(dd3,sp=="ooshirabiso")
tn<-nrow(d)
d[,clm_f]



