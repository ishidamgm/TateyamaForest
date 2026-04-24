# ______________________________________________________
# Abies mariesii 累積死亡率・BA比 vs. WI / year の解析
# S3クラス "statistics" を用いたリファクタリング版
# ______________________________________________________
# mortality_ba_ancova.R


# ______________________________________________________
# ユーティリティ関数（内部使用）
# ______________________________________________________

#' 2グループ間の傾き差をANCOVAで検定（内部関数）
#'
#' @param data      data.frame
#' @param response  目的変数名（文字列）
#' @param covariate 共変量名（文字列）
#' @param groupvar  グループ変数名（文字列）
#' @param ref       参照グループ
#' @param other     比較グループ
#' @returns named numeric vector
pairwise_slope <- function(data, response, covariate, groupvar, ref, other) {
  data$grp <- factor(data[[groupvar]],
                     levels = c(ref, setdiff(unique(data[[groupvar]]), ref)))
  fml  <- as.formula(paste(response, "~", covariate, "* grp"))
  m    <- lm(fml, data = data)
  cf   <- summary(m)$coefficients
  row  <- paste0(covariate, ":grp", other)
  b    <- coef(m)
  c(slope_ref   = unname(b[covariate]),
    slope_other = unname(b[covariate]) + unname(b[row]),
    estimate    = unname(cf[row, "Estimate"]),
    t           = unname(cf[row, "t value"]),
    p           = unname(cf[row, "Pr(>|t|)"]))
}

#' 全ペアの傾き比較（内部関数）
#'
#' @param data      data.frame
#' @param response  目的変数名（文字列）
#' @param covariate 共変量名（文字列）
#' @param groupvar  グループ変数名（文字列）
#' @returns data.frame（結果表）
run_pairwise <- function(data, response, covariate, groupvar) {
  groups <- levels(factor(data[[groupvar]]))
  pairs  <- combn(groups, 2, simplify = FALSE)
  n_pairs <- length(pairs)

  results <- data.frame()
  for (pair in pairs) {
    res    <- pairwise_slope(data, response, covariate, groupvar,
                              pair[1], pair[2])
    results <- rbind(results, data.frame(
      comparison  = paste(pair[1], "vs", pair[2]),
      slope_ref   = round(res["slope_ref"],   5),
      slope_other = round(res["slope_other"], 5),
      slope_diff  = round(res["estimate"],    5),
      t_value     = round(res["t"],           3),
      p_value     = round(res["p"],           4)
    ))
  }
  rownames(results) <- NULL
  results
}


# ______________________________________________________
# S3クラス "statistics" のコンストラクタ
# ______________________________________________________

#' statisticsオブジェクトの生成
#'
#' データの前処理（WI結合・factor設定）を一元化する。
#'
#' @param wi_year         年別WIのdata.frame（列: year, Kaminokodaira, Matsuotoge, Kagamiishi）
#' @param Abies_death_ratio プロット別累積死亡率リスト（各要素: data.frame with year, death_ratio）
#' @param f1_raw          Kaminokodaira 樹種別BA比 data.frame（列: year, sp, ba_ratio）
#' @param f2_raw          Kaminokodaira zone別BA比 data.frame（列: year, sp, ba_ratio）
#' @returns "statistics" クラスオブジェクト
#' @export
#'
#' @examples
#' s <- new_statistics(wi_year, Abies_death_ratio,
#'                     .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
#'                     .data_Fig_yr_ba_kaminokodaira_zone_2024)
new_statistics <- function(wi_year, Abies_death_ratio, f1_raw, f2_raw) {

  ## Kaminokodaira 調査年のWI
  kami_years <- sort(unique(f1_raw$year))
  wi_kami    <- data.frame(
    year = kami_years,
    WI   = wi_year[match(kami_years, wi_year$year), "Kaminokodaira"]
  )

  ## 樹種別・zone別BAにWIを結合
  f1      <- f1_raw
  f2      <- f2_raw
  f1$WI   <- wi_kami$WI[match(f1$year, wi_kami$year)]
  f2$WI   <- wi_kami$WI[match(f2$year, wi_kami$year)]
  f1$sp   <- factor(f1$sp,
               levels = c("Fagus crenata",
                          "Cryptomeria japonica",
                          "Abies mariesii"))
  f2$sp   <- factor(f2$sp,
               levels = c("Temperate tree species",
                          "Ecotone tree species",
                          "Subarctic tree species"))

  ## 死亡率データにWIを結合・結合
  death_list <- lapply(names(Abies_death_ratio), function(plot) {
    df      <- Abies_death_ratio[[plot]]
    df$WI   <- wi_year[match(df$year, wi_year$year), plot]
    df$plot <- plot
    df
  })
  death_data       <- do.call(rbind, death_list)
  death_data$plot  <- factor(death_data$plot,
                       levels = c("Kaminokodaira", "Matsuotoge", "Kagamiishi"))

  structure(
    list(
      wi         = wi_year,
      wi_kami    = wi_kami,
      death_data = death_data,
      f1         = f1,
      f2         = f2
    ),
    class = "statistics"
  )
}

#' print メソッド
#' @export
print.statistics <- function(x, ...) {
  cat("=== statistics object ===\n")
  cat(sprintf("WI period   : %d -- %d\n",
              min(x$wi$year), max(x$wi$year)))
  cat(sprintf("Death data  : n = %d (%s)\n",
              nrow(x$death_data),
              paste(levels(x$death_data$plot), collapse = ", ")))
  cat(sprintf("BA (species): n = %d\n", nrow(x$f1)))
  cat(sprintf("BA (zone)   : n = %d\n", nrow(x$f2)))
  invisible(x)
}


# ______________________________________________________
# 統計関数群
# ______________________________________________________

#' 1. Pearson相関: WI vs. Abies 累積死亡率（各プロット・プール）
#'
#' @param s "statistics" オブジェクト
#' @returns data.frame（plot, r, p, n）
#' @export
#'
#' @examples
#'  s <- new_statistics(
#' wi_year          = wi_year,
#' Abies_death_ratio = Abies_death_ratio,
#' f1_raw           = .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
#' f2_raw           = .data_Fig_yr_ba_kaminokodaira_zone_2024
#' )
#'
#' (res<-statistics_cor_WI_AbiesCumulativeMortality(s))
#' str(res)
#'
statistics_cor_WI_AbiesCumulativeMortality <- function(s) {
  stopifnot(inherits(s, "statistics"))
  message("Pearson correlation: WI vs. cumulative mortality")

  d <- lapply(levels(s$death_data$plot), function(plot) {
    df <- s$death_data[s$death_data$plot == plot, ]
    ct <- cor.test(df$WI, df$death_ratio, method = "pearson")
    data.frame(plot = plot, r = ct$estimate, p = ct$p.value, n = nrow(df))
  })
  d <- do.call(rbind, d)

  ct_all <- cor.test(s$death_data$WI, s$death_data$death_ratio,
                     method = "pearson")
  result <- rbind(d,
    data.frame(plot = "Pooled", r = ct_all$estimate,
               p = ct_all$p.value, n = nrow(s$death_data)))
  rownames(result) <- NULL
  result
}


#' 2. ANCOVA: 累積死亡率 ~ year * plot（傾きのペアワイズ比較）
#'
#' @param s "statistics" オブジェクト
#' @returns list（model summary, slopes, pairwise comparisons）
#' @export
#'
#' @examples
#'  s <- new_statistics(
#' wi_year          = wi_year,
#' Abies_death_ratio = Abies_death_ratio,
#' f1_raw           = .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
#' f2_raw           = .data_Fig_yr_ba_kaminokodaira_zone_2024
#' )
#'
#' (res<-statistics_ANCOVA_Abies_CumulativeDeathRatio_year_plot(s))
#' str(res)
#'
statistics_ANCOVA_Abies_CumulativeDeathRatio_year_plot <- function(s) {
  stopifnot(inherits(s, "statistics"))
  message("ANCOVA: cumulative mortality ~ year * plot")

  m <- lm(death_ratio ~ year * plot, data = s$death_data)
  print(summary(m))

  b      <- coef(m)
  slopes <- c(
    Kaminokodaira = unname(b["year"]),
    Matsuotoge    = unname(b["year"]) + unname(b["year:plotMatsuotoge"]),
    Kagamiishi    = unname(b["year"]) + unname(b["year:plotKagamiishi"])
  )
  cat("\n--- Slope of each plot ---\n")
  for (nm in names(slopes))
    cat(sprintf("  %-20s %.6f /yr\n", nm, slopes[nm]))

  pw <- .run_pairwise(s$death_data, "death_ratio", "year", "plot")
  cat("\n--- Pairwise slope comparisons (death_ratio ~ year) ---\n")
  print(pw)

  invisible(list(model = m, slopes = slopes, pairwise = pw))
}


#' 3. Pearson相関: WI vs. BA比（樹種別・zone別、Kaminokodaira）
#'
#' @param s "statistics" オブジェクト
#' @returns list（by_species, by_zone）各data.frame（sp, r, p）
#' @export
#'
#' @examples
#'  s <- new_statistics(
#' wi_year          = wi_year,
#' Abies_death_ratio = Abies_death_ratio,
#' f1_raw           = .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
#' f2_raw           = .data_Fig_yr_ba_kaminokodaira_zone_2024
#' )
#' (res<-statistics_cor_wi_ba_EcotonePlot(s))
#' str(res)
#'
statistics_cor_wi_ba_EcotonePlot <- function(s) {
  stopifnot(inherits(s, "statistics"))
  message("Pearson correlation: WI vs. BA ratio (Kaminokodaira)")

  cor_by_group <- function(data, group_var) {
    do.call(rbind, lapply(levels(data[[group_var]]), function(g) {
      df <- data[data[[group_var]] == g, ]
      ct <- cor.test(df$WI, df$ba_ratio, method = "pearson")
      data.frame(group = g, r = ct$estimate, p = ct$p.value, n = nrow(df))
    }))
  }

  by_sp   <- cor_by_group(s$f1, "sp")
  by_zone <- cor_by_group(s$f2, "sp")
  rownames(by_sp) <- rownames(by_zone) <- NULL

  cat("\n--- By species ---\n");   print(by_sp)
  cat("\n--- By zone ---\n");      print(by_zone)

  invisible(list(by_species = by_sp, by_zone = by_zone))
}


#' 4. ANCOVA: BA比 ~ WI * 樹種/zone（傾きのペアワイズ比較）
#'
#' @param s "statistics" オブジェクト
#' @returns list（by_species, by_zone）各list（model, slopes, pairwise）
#' @export
#'
#' @examples
#'  s <- new_statistics(
#' wi_year          = wi_year,
#' Abies_death_ratio = Abies_death_ratio,
#' f1_raw           = .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
#' f2_raw           = .data_Fig_yr_ba_kaminokodaira_zone_2024
#' )
#' (res<-statistics_ANCOVA_wi_ba_EcotonePlot(s))
#' str(res)
statistics_ANCOVA_wi_ba_EcotonePlot <- function(s) {
  stopifnot(inherits(s, "statistics"))
  message("ANCOVA: BA ratio ~ WI * species/zone")

  run_ancova <- function(data, label) {
    cat(sprintf("\n--- %s ---\n", label))
    m  <- lm(ba_ratio ~ WI * sp, data = data)
    print(summary(m))
    b  <- coef(m)
    sp_levels <- levels(data$sp)

    slopes <- sapply(sp_levels, function(sp) {
      int_nm <- paste0("WI:sp", sp)
      unname(b["WI"]) + ifelse(int_nm %in% names(b), unname(b[int_nm]), 0)
    })

    cat("--- Slope of each group ---\n")
    for (nm in names(slopes))
      cat(sprintf("  %-28s %.5f /WI\n", nm, slopes[nm]))

    pw <- .run_pairwise(data, "ba_ratio", "WI", "sp")
    cat("\n--- Pairwise slope comparisons (ba_ratio ~ WI) ---\n")
    print(pw)

    invisible(list(model = m, slopes = slopes, pairwise = pw))
  }

  res_sp   <- run_ancova(s$f1, "By species")
  res_zone <- run_ancova(s$f2, "By zone")

  invisible(list(by_species = res_sp, by_zone = res_zone))
}


# ______________________________________________________
# 使用例
# # ______________________________________________________
# if (FALSE) {
#   # オブジェクト生成
#   s <- new_statistics(
#     wi_year          = wi_year,
#     Abies_death_ratio = Abies_death_ratio,
#     f1_raw           = .data_Fig_yr_ba_kaminokodaira_Cryptomeria_Fagus_Abies_2024,
#     f2_raw           = .data_Fig_yr_ba_kaminokodaira_zone_2024
#   )
#
#   print(s)
#
#   # 各統計の実行
#   res1 <- statistics_cor_WI_AbiesCumulativeMortality(s)
#   res2 <- statistics_ANCOVA_Abies_CumulativeDeathRatio_year_plot(s)
#   res3 <- statistics_cor_wi_ba_EcotonePlot(s)
#   res4 <- statistics_ANCOVA_wi_ba_EcotonePlot(s)
#
#   # 結果の取り出し例
#   res2$pairwise        # ペアワイズ比較表
#   res4$by_species$slopes  # 樹種別傾き
# }
