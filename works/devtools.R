# devtools.R

getwd()

fs::dir_tree()



# rm(list=ls())
# ls()

# devtools::install_github("ishidamgm/TateyamaForest")
library(TateyamaForest)
help(package="TateyamaForest")
data(package="TateyamaForest")
dd3
clm_f
clm_dbh
plt[,clm_yr]

## restore the saved values to the current environment
local({
  load("data/TateyamaForest_dd3_plt.RData")
  ls()
})

#
help.start()
tools::Rd2pdf("~/Dropbox/00D/00/tateyama/TateyamaForest")
# R CMD Rd2pdf  "~/Dropbox/00D/00/tateyama/TateyamaForest"
system("pdflatex --version")

devtools::build_manual()

