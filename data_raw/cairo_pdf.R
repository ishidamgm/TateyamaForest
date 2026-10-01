# cairo_pdf.R

# Fig7 ####
tiff("Fig7.tiff", width = 3000, height = 1800, res = 300, compression = "lzw")
res<-Fig_wi_ba_cor_ancova_JVS2()
dev.off()

cairo_pdf("Fig7.pdf", width = 7, height = 5.5)
res<-Fig_wi_ba_cor_ancova_JVS2()
dev.off()

# Fig6 ####

#' \preformatted{
#' tiff("Fig6.tiff", width = 3000, height = 1800, res = 300, compression = "lzw")
#' cairo_pdf("Fig6.pdf", width = 7, height = 5.5) #,pointsize=8, fallback_resolution = 300
#' par(ps  = 7,    # 基準を9ptに
#' cex = 0.8, # 倍率は1.0のまま
#' cex.axis = 0.7,   # 軸数字 → 8.1pt
#' cex.lab  = 0.7,   # 軸ラベル → 9pt
#' mar = c(4, 4.5, 1, 1))
#'
#' TateyamaForest::Fig_yr_ba_Kaminoko_JVS2()
#' dev.off()
#' #  FIGURE 6 | Changes in basal area in the Ecotone plot over time.
#' }

# Fig.5 ####$
#' \preformatted{
#' tiff("Fig5.tiff", width = 2000, height = 1700, res = 300, compression = "lzw")
#' cairo_pdf("Fig5.pdf", width = 6, height = 5.5)
#' TateyamaForest::Fig_yr_ba_site2_zone()
#' dev.off()
#' # FIGURE 5 | Changes in the total basal area in each plot over time.
#' }
#'
#'
# Fig4. ####
#' \preformatted{
#' tiff("Fig4.tiff", width = 3000, height = 2500, res = 300, compression = "lzw")
#' par(ps  = 9,    # 基準を9ptに
#' cex = 1.0,  # 倍率は1.0のまま
#' cex.axis = 0.9,   # 軸数字 → 8.1pt
#' cex.lab  = 1.0,   # 軸ラベル → 9pt
#' mar = c(4, 4.5, 1, 1))
#' cairo_pdf("Fig4.pdf", width = 7.09, height = 5.5)
#' Fig_year_WI_3()
#'dev.off()
#' cairo_pdf("Fig3.pdf", width = 6, height = 5.5)
#'
#'
setwd("/home/i/8T/Dropbox/00D/00/tateyama/TateyamaForest/works2/JournalVegetationScience/!JVS四稿提出/Fig/")
f<-dir()
file.info(f)

# tiff ####
library(magick)

image_read(f[3])

f <- list.files(pattern = "\\.tiff$")
info <- lapply(f, function(x) {
  img <- image_read(x)
  print(img)
  inf <- image_info(img)
  data.frame(
    file     = x,
    width_px = inf$width,
    height_px= inf$height,
    density  = inf$density,   # dpi情報（埋め込まれていれば）
    filesize = file.info(x)$size
  )
})
do.call(rbind, info)

# pdf ####
cairo_pdf("Fig4.pdf", width = 7.09, height = 5.00)
par(ps  = 9,    # 基準を9ptに
    cex = 1.0,  # 倍率は1.0のまま
    cex.axis = 0.9,   # 軸数字 → 8.1pt
    cex.lab  = 1.0,   # 軸ラベル → 9pt
    mar = c(4, 4.5, 1, 1))
Fig_year_WI_3()
dev.off()

# Fig.1
# 方法1: gimp　画像→統合→サイズ300*300→情報確認→エクスポート　lzw圧縮

image_read("Fig1.tiff")
