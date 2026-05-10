#
#' Fig_year_WI_JP_3
#'
#' @return
#' @export
#'
#' @examples
#'  res<-Fig_year_WI_3()
#'  res
#'
Fig_year_WI_3<-function(){
  intact_plot <- c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")
  plot_no <- match(intact_plot,colnames(wi_year))
  legend_no <- match(intact_plot,leg$n)
  par(mgp=c(2.5, 1, 0))
  plot(0,type="n",xlab="Year", ylab="Warmth Index (°C·month)",
       xlim=c(1960,2025),ylim=c(8,87),cex.lab=1.1,cex.axis=1.1)

  text(1960,72,"Temperate zone",cex=1.2, adj = 0, font = 3,  col = "grey60")
  text(1960,47,"Ecotone",cex=1.2, adj = 0, font = 3,  col = "grey60")
  text(1960,33,"Subarctic zone",,cex=1.2, adj = 0, font = 3,  col = "grey60")


  abline(h=c(15,45,55,85),lty=2,lwd=3,col="red")

  wi.<-wi_year #WI_with_KurodeDamObservation
  res<-c()
  for (i in 1:length(plot_no)){
    x<-wi.$year
    y<-wi.[,plot_no[i]]
    res.<-lm(y~x)
    res<-c(res,list(res.))
    lines(x,y,type="l",lty=i, lwd=2,#pch=i,
          col=leg$col[legend_no[i]])
    abline(res.)
  }
names(res)<-intact_plot

  for (ii in match(intact_plot,plt6$na)){
    yr.<-plt6[ii,paste0("yr",1:7)]
    wi.<-plt6[ii,paste0("wi",1:7)]
    points(yr.,wi.)
    text(yr.-0,wi.+2,1:7,cex=1)
  }


  legend(2003, 24, plot_name,
         cex = 0.9,
         lty = 1:4,
         lwd = 2,
         col = leg$col[c(2, 4, 5, 7)],
         y.intersp = 0.7,    # 行間（デフォルト1.0）
         x.intersp = 0.2,    # 線とテキストの間隔
         box.lwd   = 0.5,    # 枠線の太さ
         inset     = 0.005    # 枠内余白
  )

  invisible(res)
}

# old ###############################
#
#' Fig_year_WI_JP_3
#'
#' @return
#' @export
#'
#' @examples
#'  Fig_year_WI_3()
#'
Fig_year_WI_3<-function(){
  intact_plot <- c("Bunazaka","Kaminokodaira","Matsuotoge","Kagamiishi")
  plot_no <- match(intact_plot,colnames(wi_year))
  legend_no <- match(intact_plot,leg$n)
  par(mgp=c(2.5, 1, 0))
  plot(0,type="n",xlab="Year", ylab="Warmth Index (°C·month)",
       xlim=c(1960,2025),ylim=c(8,87),cex.lab=1.1,cex.axis=1.1)

  text(1960,72,"Temperate zone",cex=1.2, adj = 0, font = 3,  col = "grey60")
  text(1960,47,"Ecotone",cex=1.2, adj = 0, font = 3,  col = "grey60")
  text(1960,33,"Subarctic zone",,cex=1.2, adj = 0, font = 3,  col = "grey60")


  abline(h=c(15,45,55,85),lty=2,lwd=3,col="red")
  wi.<-wi_year #WI_with_KurodeDamObservation
  for (i in 1:length(plot_no)){
    lines(wi.$year,wi.[,plot_no[i]],type="l",lty=i, lwd=2,#pch=i,
          col=leg$col[legend_no[i]])

  }
  #______________
  #lm(wi.[,plot_no[i]]~wi.$year)
  # Mann-Kendall検定
  # mk <- MannKendall(wi)
  #
  # # 線形回帰（傾き/decade）
  # lm_fit <- lm(wi ~ years)
  # slope_decade <- coef(lm_fit)[2] * 10
  #
  # cat(name, "\n")
  # cat("  z =", qnorm(mk$sl/2, lower.tail = FALSE), "\n")
  # cat("  p =", mk$sl, "\n")
  # cat("  slope =", round(slope_decade, 2), "WI per decade\n\n")


  #______________

  for (ii in match(intact_plot,plt6$na)){
    yr.<-plt6[ii,paste0("yr",1:7)]
    wi.<-plt6[ii,paste0("wi",1:7)]
    points(yr.,wi.)
    text(yr.-0,wi.+2,1:7,cex=1)
  }


  legend(2003, 24, plot_name,
         cex = 0.9,
         lty = 1:4,
         lwd = 2,
         col = leg$col[c(2, 4, 5, 7)],
         y.intersp = 0.7,    # 行間（デフォルト1.0）
         x.intersp = 0.2,    # 線とテキストの間隔
         box.lwd   = 0.5,    # 枠線の太さ
         inset     = 0.005    # 枠内余白
  )
}
