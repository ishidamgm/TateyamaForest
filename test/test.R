library(dplyr)
nasa
data("nasa", package = "dplyr")
nasa

library(ggplot2)
data(package="ggplot2")
?diamonds

data(package="TateyamaForest")
ls()
dir("data/")
data(load("data/TateyamaForest_dd3_plt.RData"))
## restore the saved values to the current environment
local({
  load("data/TateyamaForest_dd3_plt.RData")
  ls()
})

library(fs)
dir_tree()

data(TateyamaForest_dd3_plt)
data()
rm(list=ls())
ls()
data()
data(package="raster")

data(package="nenrin")
data(package="RGBFisheye")
data(package="ForestTools")
data(package="TateyamaForest")
library(TateyamaForest)
dd2

library(sf)
demo(package = "sf")

URL.CURRENT <- "http://api.openweathermap.org/data/2.5/weather?"
current.Weather <- getURL(paste0(URL.CURRENT,"q=","Tokyo"))
fromJSON(current.Weather);

z<-c("a","b","g")
for(i in 1:length(z))assign(z[i],1:10)
g
letters
a<-tibble(tibble(x = 1:3, y = list(1:5, 1:10, 1:20)))
          #> # A tibble: 3 × 2)
a
str(a)
a$y[1]



library(tidyverse)
library(gtrendsR)
library(rvest)
library(ngramr)

trend <- gtrends(keyword = "コロナ", geo = "JP")
plot(trend)

trend <- gtrends(keyword = c("ロシア", "ウクライナ"), geo = "JP")
plot(trend)



