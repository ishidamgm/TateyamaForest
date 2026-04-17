# Tateyama_Nakajima_statistics.R ####
# 中島さんのコメントに対する検討

# JVS2 ####
# オオシラビソの死亡率　離散と瞬間

# instantaneous
Population_growth("Kaminokodaira")
#' #11   オオシラビソ  30  17  15 24 -2.3666002 2.8881133  0.5215131
DemographicRates(n0=30, n1=17, nS=15,t=24)
# n0 n1 nS  t       p        m         r
# 1 30 17 15 24 -2.3666 2.888113 0.5215131
log(30/15)/24

# discreate　 1 - (1 - p)^(1/d)
Abies_death_ratio$Kaminokodaira
MortalityRate(0.50000000,24)   # 0.02846806
1 - (1 - 15/30)^(1/24)


#' discrete Mortality rate
#'  "discrete"   1 - (1 - p)^(1/d)
#'  "instantaneous"  log(1/p)/d
#'
#' @param p proportion of trees that DIED during the interval
#' @param d duration
#' @param method "discrete"(fefault),"instantaneous"
#'
#' @returns Mortality Rate
#'
#' @export
#'
#' @examples
#' Abies_death_ratio
#' MortalityRate(0.48000000,24,method="discrete")
#' # 0.02687909
#' 1-(1-0.02687909)^24
#' MortalityRate(0.48000000,24,method="instantaneous")
#'  #0.03058205
#'  exp(-0.03058205*24)
#'
#'
MortalityRate<-function(p,d,method=c("discrete","instantaneous")){
  method <- match.arg(method)

  result <- switch(method,
                   "discrete" = {
                     1 - (1 - p)^(1/d)
                   },
                   "instantaneous" = {
                     log(1/p)/d
                   }
  )

  return(result)

}



## 黒部ダムの気温とwi　##############################
plot(kurobe_dam_temperature$min,d.wi$Matsuotoge)


# 温量指数読み込み
d.wi<-as.data.frame(WI_with_KurodeDamObservation)
# !!要注意　2024データが入手できた時点で修正　####
# 2024は2023とほぼ同レベルの気温　（Nakajime2026）
d.wi<-rbind(d.wi,d.wi[nrow(d.wi),])
rownames(d.wi)[nrow(d.wi)]<-2024
# 中島さんのデータから補正
t2024=　wi2023+(t2024-t2023)*(wi2022-wi2023)/(t2022-t2023)
#ブナ坂
t2024=9.9;wi2023=75.132;wi2022=71.732;t2023=10.0;t2022=9.0
wi2023+(t2024-t2023)*(wi2022-wi2023)/(t2022-t2023)
#[1] 74.792   d.wi$Bunazaka[rownames(d.wi)==2024]<-74.792
#Kaminokodaira
t2024=9.9;wi2023= 42.19500 ;wi2022= 38.48400 ;t2023=4.4;t2022=3.5
wi2023+(t2024-t2023)*(wi2022-wi2023)/(t2022-t2023)
#[1]  64.87333 d.wi$Kaminokodaira[rownames(d.wi)==2024]<- 64.87333

#Matsuotoge
t2024=4.8;wi2023= 42.19500 ;wi2022= 38.48400 ;t2023=4.4;t2022=3.5
wi2023+(t2024-t2023)*(wi2022-wi2023)/(t2022-t2023)
#[1]  43.84433  d.wi$Matsuotoge[rownames(d.wi)==2024]<- 43.84433

#Kagamiishi
t2024=4.8;wi2023= 34.49500 ;wi2022= 30.49600 ;t2023=4.4;t2022=3.5
wi2023+(t2024-t2023)*(wi2022-wi2023)/(t2022-t2023)
#[1]  36.27233  d.wi$Kagamiishi[rownames(d.wi)==2024]<- 36.27233

# write.csv(d.wi,file="wi2024.csv")

# 区間平均検討　（保留）　####
yr<-as.numeric(rownames(d.wi))
# 　調査地
plot.name<- c("Kaminokodaira","Matsuotoge","Kagamiishi")
ii<-1
pn<-plot.name[ii]
#　調査年
d<-Abies_death_ratio
yr.interval<-d[[pn]]$year


#　温量指数の調査年区間平均
wi.　<-　YearsIntervalAverage(yr,d.wi[,pn],yr.interval)

#　計測値の調査年区間平均
v　<-　d[[pn]]$death_ratio
plot(wi.,v)


Abies.death.ratio.YearsIntervalAverage<-YearsIntervalAverage(yr,v,yr.interval)

plot(Abies.death.ratio.YearsIntervalAverage)
plot(wi.Kagamiishi)

# 年区間平均 ####

#' YearsIntervalAverage
#'
#' @param yr
#' @param v
#' @param yr.interval
#'
#' @returns
#'
#' @export
#'
#' @examples
#' plot_name<-"Kaminokodaira"
#' yr.interval<-as.numeric(subset(plt5,na==plot.name)[,paste0("yr",1:7)])
#' yr<-as.numeric(rownames(wi_year))
#' v<-as.numeric(wi_year[,plot_name])
#' YearsIntervalAverage(yr,v,yr.interval)
#'
YearsIntervalAverage<-function(yr,v,yr.interval){
  m<-c()
  n.yr<-match(yr.interval,yr)
  for(i in 1:(length(n.yr)-1)){
   m<-c(m,mean(v[n.yr[i]:n.yr[i+1]],na.rm=TRUE))
  }
  return(m)
}



# 解析データ　####

# wi ####
read.csv("wi2024.csv")
## A.mariesii death ratio ####
Abies_death_ratio


# Cumulative mortality analysis of Abies mariesii

# ANCOVA: death_ratio ~ year * plot ####


## --- 関数定義 ------------------------------------------------
MortalityRate <- function(p, d, method = c("instantaneous", "discrete")) {
  method <- match.arg(method)
  switch(method,
         "instantaneous" = log(1 / p) / d,   # ln(N0/Ns)/t
         "discrete"      = 1 - (1 - p)^(1 / d)
  )
}

## --- データ読み込み ------------------------------------------
load("Abies_death_ratio.RData")   # Abies_death_ratio (list)
wi <- read.csv("wi2024.csv")
names(wi)[1] <- "year"

## --- 各プロットにWIを結合 ------------------------------------
get_data <- function(plot) {
  df      <- Abies_death_ratio[[plot]]
  df$WI   <- wi[match(df$year, wi$year), plot]
  df$plot <- plot
  df
}

Kami  <- get_data("Kaminokodaira")
Matsu <- get_data("Matsuotoge")
Kaga  <- get_data("Kagamiishi")

all_data       <- rbind(Kami, Matsu, Kaga)
all_data$plot  <- factor(all_data$plot,
                         levels = c("Kaminokodaira", "Matsuotoge", "Kagamiishi"))


## 1. WI vs. 累積死亡率  Pearson相関（各プロット n=7） ####

cat("=== 1. Pearson correlation: WI vs. cumulative mortality ===\n")
for (plot in levels(all_data$plot)) {
  df <- all_data[all_data$plot == plot, ]
  ct <- cor.test(df$WI, df$death_ratio, method = "pearson")
  cat(sprintf("%-15s r = %6.3f, p = %.4f\n", plot, ct$estimate, ct$p.value))
}

cat("\n--- Pooled (n=21) ---\n")
ct_all <- cor.test(all_data$WI, all_data$death_ratio, method = "pearson")
cat(sprintf("Pooled         r = %6.3f, p = %.4f\n", ct_all$estimate, ct_all$p.value))


## 2. ANCOVA: 累積死亡率 ~ year * plot ####

cat("\n=== 2. ANCOVA: death_ratio ~ year * plot ===\n")
m <- lm(death_ratio ~ year * plot, data = all_data)
print(summary(m))

# --- 各プロットの回帰直線の傾き ---
b_kami  <- coef(m)["year"]
b_matsu <- coef(m)["year"] + coef(m)["year:plotMatsuotoge"]
b_kaga  <- coef(m)["year"] + coef(m)["year:plotKagamiishi"]

cat("--- Slope of each plot (cumulative mortality/yr) ---\n")
cat(sprintf("Kaminokodaira (Ecotone)  : %.6f\n", b_kami))
cat(sprintf("Matsuotoge    (Subarctic): %.6f\n", b_matsu))
cat(sprintf("Kagamiishi    (Timberline): %.6f\n", b_kaga))


## 3. ペアワイズ傾きの比較（参照水準を変えて再フィット） ####

cat("\n=== 3. Pairwise slope comparisons ===\n")

pairwise_slope <- function(data, ref, other) {
  data$pl <- factor(data$plot, levels = c(ref, setdiff(levels(data$plot), ref)))
  m_tmp   <- lm(death_ratio ~ year * pl, data = data)
  cf      <- summary(m_tmp)$coefficients
  row     <- paste0("year:pl", other)
  c(estimate = cf[row, "Estimate"],
    t        = cf[row, "t value"],
    p        = cf[row, "Pr(>|t|)"])
}

pairs <- list(
  c("Kaminokodaira", "Matsuotoge"),
  c("Kaminokodaira", "Kagamiishi"),
  c("Matsuotoge",    "Kagamiishi")
)

results <- data.frame()
for (pair in pairs) {
  res <- pairwise_slope(all_data, pair[1], pair[2])
  cat(sprintf("%s vs. %s:\n  slope diff = %.6f, t = %.3f, p = %.4f",
              pair[1], pair[2], res["estimate"], res["t"], res["p"]))
  # Bonferroni補正
  p_bonf <- min(res["p"] * 3, 1)
  cat(sprintf(", p_Bonferroni = %.4f\n", p_bonf))
  results <- rbind(results, data.frame(
    comparison    = paste(pair[1], "vs", pair[2]),
    slope_diff    = round(res["estimate"], 6),
    t_value       = round(res["t"], 3),
    p_value       = round(res["p"], 4),
    p_Bonferroni  = round(p_bonf, 4)
  ))
}

cat("\n--- Summary table ---\n")
print(results, row.names = FALSE)


## A.mariesii BA  ####
sp_ba
sp_ba_ratio
