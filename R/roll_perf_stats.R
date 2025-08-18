#' Rolling performance & factor statistics for a strategy vs. benchmark
#'
#' Computes **sliding-window** (rolling) performance and factor statistics for a
#' single strategy series tested against a benchmark, using Fama–French 3 factors
#' and the risk-free rate. The function aligns all inputs to the selected
#' \code{frequency}, merges them on \code{date}, cleans missing data via
#' \code{ZZ_remove_na()}, and then applies rolling regressions and performance metrics.
#'
#' @details
#' \itemize{
#'   \item \strong{Input strategy:} \code{df_merged} must contain a \code{date} column
#'         and exactly one additional (numeric) column with the strategy \emph{period} returns
#'         (simple returns), at daily granularity. The function will resample to weekly/monthly
#'         if requested.
#'   \item \strong{Benchmark:} Retrieved via \code{ZZ_benchmark()} at the same frequency.
#'   \item \strong{Fama–French:} Downloaded via \code{download_french_data("Fama/French 3 Factors [Daily]")},
#'         converted to proportions (divided by 100), and resampled to match \code{frequency}.
#'   \item \strong{Cleaning:} After merging, the data are cleaned with \code{ZZ_remove_na(..., protect = "date")}.
#'   \item \strong{Rolling window:} Window length and annualization use
#'         \code{252} (daily), \code{52} (weekly), \code{12} (monthly).
#'         Windows slide by one period (i.e., \code{by = 1} in \code{zoo::rollapply}).
#'   \item \strong{Metrics:} CAPM and FF3 alphas/betas (alphas annualized),
#'         annualized excess return/volatility, Sharpe, CAGR, tracking error,
#'         information ratio, Sortino, high-water mark, max drawdown, bootstrap VaR
#'         and ES (daily, with simple annualized approximations).
#' }
#'
#' @param df_merged \code{data.frame} with columns \code{date} (Date) and exactly one
#'   numeric strategy returns column (period simple returns). Any additional columns
#'   besides \code{date} will trigger an error.
#' @param start_date,end_date Date bounds (inclusive) for the sample.
#' @param conf_level Confidence level for VaR/ES and summary CIs (default \code{0.95}).
#' @param frequency One of \code{"daily"}, \code{"weekly"}, \code{"monthly"}.
#'   Controls resampling, rolling window length, and annualization factor.
#' @param mar Minimum acceptable return for Sortino ratio (default \code{0}).
#' @param n_boot Number of bootstrap draws for VaR/ES (default \code{1000}).
#'
#' @return A \code{list} with two elements:
#' \describe{
#'   \item{\code{rolling}}{A data.frame of rolling statistics, one row per window end date.}
#'   \item{\code{summary}}{A data.frame summarizing the rolling metrics across windows
#'         (mean, SD, t-stat, two-sided p-value, and t-based confidence interval).}
#' }
#'
#' @section Assumptions:
#' \itemize{
#'   \item Strategy returns in \code{df_merged} are \emph{period simple returns}.
#'   \item Risk-free and FF3 factors are in the same periodicity after resampling.
#'   \item VaR/ES annualization uses simple square-root/time scaling approximations.
#' }
#'
#' @examples
#' \dontrun{
#' # df_strat must have columns: date, Strategy
#' out <- roll_perf_stats(
#'   df_merged  = df_strat,
#'   start_date = as.Date("2015-01-01"),
#'   end_date   = Sys.Date(),
#'   frequency  = "monthly",
#'   conf_level = 0.95,
#'   n_boot     = 1000
#' )
#'
#' head(out$rolling)
#' out$summary
#' }
#'
#' @seealso \code{\link{ZZ_benchmark}}, \code{\link{ZZ_remove_na}}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate rename_with filter select across
#' @importFrom stringr str_to_lower
#' @importFrom lubridate ymd
#' @importFrom xts xts to.period
#' @importFrom zoo rollapply index coredata
#' @importFrom stats lm coef qt pt quantile sd
#' @importFrom frenchdata download_french_data
#' @export
#'
roll_perf_stats <- function(df_merged,
                            start_date,
                            end_date,
                            conf_level = 0.95,
                            frequency = c("daily", "weekly", "monthly"),
                            mar = 0,
                            n_boot = 1000) {


  # General Inputs
  frequency <- match.arg(frequency)
  window_map <- c(daily = 252, weekly = 52, monthly = 12)
  trading_days <- window <- window_map[[frequency]]


  ### DF for the Rollapply Analysis ###

  # 1. Input df for strategy to backtest
  df_merged$date <- as.Date(df_merged$date)
  strat <- setdiff(colnames(df_merged), "date")
  df_sub <- subset(df_merged, date >= start_date & date <= end_date)

  if (length(strat) > 1) stop("df_merged must have exactly one strategy column besides 'date'")

  df_sub_xts <- xts(df_sub[,-1], order.by = df_sub$date)
  if (frequency == "weekly") {
    df_sub <- to.period(df_sub_xts, period = "weeks", indexAt = "endof", OHLC = FALSE)
  } else if (frequency == "monthly") {
    df_sub <- to.period(df_sub_xts, period = "months", indexAt = "endof", OHLC = FALSE)
  }
  df_sub <- data.frame(date = index(df_sub), coredata(df_sub))

  # 2. Benchmark Returns
  ZZ_benchmark <- ZZ_benchmark(start = start_date,
                               end = end_date,
                               frequency = frequency,
                               type = "simple",
                               cum = FALSE)

  # 3. Fama French Three Factors Returns
  ff3_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")
  ff3_tbl <- ff3_daily_raw$subsets$data[[1]]

  ff3_daily <- ff3_tbl %>%
    mutate(date = ymd(date)) %>%
    mutate(across(c(RF, `Mkt-RF`, SMB, HML), ~ as.numeric(.) / 100)) %>%
    dplyr::select(date, RF, `Mkt-RF`, SMB, HML) %>%
    rename_with(str_to_lower) %>%
    rename(mkt_excess = `mkt-rf`) %>%
    filter(date >= start_date, date <= end_date)

  # Match frequency of ff3 data to chosen frequency
  ff3_xts <- xts(ff3_daily[,-1], order.by = ff3_daily$date)

  if (frequency == "weekly") {
    ff3_resampled <- to.period(ff3_xts, period = "weeks", indexAt = "endof", OHLC = FALSE)
  } else if (frequency == "monthly") {
    ff3_resampled <- to.period(ff3_xts, period = "months", indexAt = "endof", OHLC = FALSE)
  } else {
    ff3_resampled <- ff3_xts
  }
  ff3_resampled <- data.frame(date = index(ff3_resampled), coredata(ff3_resampled))

  # 4. Combine to one data frame all data
  df_sub_all <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE),
                      list(df_sub, ZZ_benchmark, ff3_resampled))

  # 5. Clean the df
  df_sub_all <- ZZ_remove_na(
    df_sub_all,
    row_thresh = 0.5,
    col_thresh = 0.5,
    interpolate = TRUE,
    protect = "date"
  )

  # 6. Create xts object
  mat_xts <- xts(df_sub_all[ , setdiff(names(df_sub_all), "date")],
                 order.by = df_sub_all$date)


  # 7. Identify columns for fama french, benchmark and for the factors
  ff_cols     <- c("rf", "mkt_excess", "smb", "hml")
  bench_col   <- "ZZ_benchmark"


  ### Rolling‐window computation ####


  res_df <- rollapply(
    mat_xts, width = window, by = 1, align = "right", fill = NA, by.column = FALSE,
    FUN = function(x) {

      ex    <- as.numeric(x[,strat]- x[,"rf"]) # Already excess returns
      bench <- as.numeric(x[,bench_col] - x[,"rf"])

      # 1. CAPM Regression

      m_1 <- lm(ex ~ as.numeric(x[,"mkt_excess"]))

      co_1 <- coef(m_1)
      names(co_1) <- c("Alpha_CAPM", "Beta_Mkt_CAPM")

      # 2. Fama-French Regression
      m_2   <- lm(ex ~ as.numeric(x[,"mkt_excess"]) +
                    as.numeric(x[,"smb"]) +
                    as.numeric(x[,"hml"]))
      co_2  <- coef(m_2)
      names(co_2) <- c("Alpha_FF3", "Beta_Mkt_FF3", "Beta_SMB", "Beta_HML")


      # 3. annualized stats
      ann_ex  <- mean(ex, na.rm=TRUE) * trading_days
      ann_vol <- sd(ex,   na.rm=TRUE) * sqrt(trading_days)
      sharpe  <- ann_ex / ann_vol
      cagr    <- prod(1 + ex)^(trading_days / window) - 1


      # 4. tracking error & information ratio
      te      <- sd(ex - bench, na.rm=TRUE) * sqrt(trading_days)
      ir      <- if (te > 0) (mean(ex - bench, na.rm=TRUE) * trading_days) / te else NA_real_

      # 5. Sortino Ratio = annual excess / annual downside dev
      downside   <- pmin(ex - mar, 0)                 # non-positive deviations
      dd_dn      <- sqrt(mean(downside^2, na.rm=TRUE)) * sqrt(trading_days)

      sortino    <- if (is.finite(dd_dn) && dd_dn > 0) ann_ex / dd_dn else NA_real_

      # 6. Bootstrap VaR
      boot_samp        <- sample(ex, size = n_boot, replace = TRUE)
      var95_boot_daily <- quantile(boot_samp, probs = 1 - conf_level, na.rm = TRUE)
      var95_boot_ann   <- var95_boot_daily * sqrt(trading_days)

      # 7. Bootstrap Expected Shortfall
      tail_boot        <- boot_samp[boot_samp <= var95_boot_daily]
      es95_boot_day    <- mean(tail_boot, na.rm = TRUE)
      es95_boot_ann    <- es95_boot_day * trading_days

      # 8. High Water Mark: maximum cumulative return in window
      cumret   <- cumprod(1 + ex) - 1
      hwm      <- max(cumret, na.rm=TRUE)

      # 9. Max Drawdown: largest peak‐to‐trough decline
      cumret_gross <- cumprod(1 + ex)
      drawdowns <- cumret_gross / cummax(cumret_gross)  -1
      mdd         <- min(drawdowns, na.rm = TRUE)

      # 10. Combine all stats

      stats <- c(date = as.Date(index(x)[nrow(x)]),
                 co_1["Alpha_CAPM"] * trading_days,
                 co_1["Beta_Mkt_CAPM"],
                 co_2["Alpha_FF3"] * trading_days,
                 co_2["Beta_Mkt_FF3"],
                 co_2["Beta_SMB"],
                 co_2["Beta_HML"],
                 Annual_Excess  = ann_ex,
                 Ann_Vol        = ann_vol,
                 Sharpe         = sharpe,
                 CAGR           = cagr,
                 Tracking_Error = te,
                 Info_Ratio     = ir,
                 Sortino_Ratio  = sortino,
                 High_Water_Mark = hwm,
                 Maximum_Drawdown = mdd,
                 VaR_95         = var95_boot_ann,
                 ES_95          = es95_boot_ann )
      stats
    }
    )

  # Summary stats
  res_df <- na.omit(as.data.frame(res_df))
  n       <- nrow(res_df)
  alpha   <- 1 - conf_level
  tcrit   <- qt(1 - alpha/2, df = n - 1)

  non_date_cols <- names(res_df)[sapply(res_df, function(x) !inherits(x, "Date"))]
  stats_df <- do.call(rbind, lapply(non_date_cols, function(col) {
    x    <- res_df[[col]]
    m    <- mean(x)
    s    <- sd(x)
    se   <- s / sqrt(n)
    ci_l <- m - tcrit * se
    ci_h <- m + tcrit * se
    tval <- m / se
    pval <- 2 * pt(-abs(tval), df = n - 1)

    data.frame(
      Metric   = col,
      Mean     = m,
      SD       = s,
      CI_Lower = ci_l,
      CI_Upper = ci_h,
      t_stat   = tval,
      p_value  = pval,
      row.names = NULL
    )
  }))

  return(list(
    rolling = res_df,
    summary = stats_df
  ))
}
