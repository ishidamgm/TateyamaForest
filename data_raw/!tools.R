# test01.R
library(tidyverse)
devtools::run_examples()
library(package="TateyamaForest")
help(package="TateyamaForest")
data(package="TateyamaForest")


# ファイル名を指定して変換する即席関数
rds_to_rda <- function(rds_path, rda_path) {
  obj_name <- tools::file_path_sans_ext(basename(rda_path)) # 拡張子を除いた名前を取得
  assign(obj_name, readRDS(rds_path))                      # その名前でオブジェクトを作成
  save(list = obj_name, file = rda_path)                   # その名前を指定して保存
}

dir(pattern=".*.rds")

rds_to_rda("kurobe_dam_temperature_2024estimated.rds", "kurobe_dam_temperature_2024estimated.RData")
rds_to_rda("ToyamaMet_1939_2025.rds", "ToyamaMet_1939_2025.RData")
rds_to_rda("YearsIntervalAverage_plots_wi.rds", "YearsIntervalAverage_plots_wi.RData")

library(clipr)
kuroba2010_2023<-read_clip_tbl()
kurobe1965_2017<-read_clip_tbl()
