#' Momentum Strategy Backtest
#'
#' Implements and backtests a cross-sectional momentum strategy on a set of ETFs
#' (or other Yahoo Finance tickers). The strategy ranks assets by their past
#' performance over a specified lookback period (with an optional skip), invests
#' in the top-\code{n} assets, optionally shorts the bottom-\code{n}, and
#' rebalances on a rolling schedule. The function produces portfolio returns,
#' turnover statistics, trades per year, benchmark comparisons, and plots.
#'
#' @param tickers A character vector of Yahoo Finance ticker symbols to include
#'   in the investment universe.
#' @param start_date Start date for the backtest (Date or character in
#'   \code{"YYYY-MM-DD"} format).
#' @param lookback_period Lookback period (in weeks) used for momentum ranking.
#'   Default is 12 months (12*4 weeks).
#' @param skip Number of weeks to skip between the end of the lookback period and
#'   the portfolio formation date (to avoid look-ahead bias). Default is 1.
#' @param top_n Number of assets to go long in at each rebalance date. Default is 2.
#' @param holding_period Number of weeks to hold each selected asset. Default is 12 weeks.
#' @param rebalance_interval Number of weeks between rebalancing the portfolio.
#'   Default is 4 (monthly).
#' @param trade_cost Fixed transaction cost per trade (applied to each position
#'   change). Default is 50.
#' @param initial_capital Starting capital for the backtest. Default is 100000.
#' @param add_benchmark Ticker of an additional benchmark (Yahoo Finance) to
#'   compare against. Default is \code{"SPY"}.
#' @param Short Logical. If \code{TRUE}, the strategy also shorts the worst
#'   \code{top_n} assets. Default is \code{TRUE}.
#' @param include_plot Logical. If \code{TRUE}, returns ggplot2 cumulative return
#'   plots for industries and strategy vs. benchmarks. Default is \code{TRUE}.
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{Summary} – Data frame with annualized return, volatility, and Sharpe ratio.
#'   \item \code{Industry Return Plot} – ggplot of cumulative industry returns (if available).
#'   \item \code{Weights} – xts object of weekly portfolio weights.
#'   \item \code{Weights this Week} – Tibble snapshot of current weights and 12m return ranks.
#'   \item \code{Turnover} – Annualized average portfolio turnover.
#'   \item \code{Trades per Year} – xts series of trade counts per year.
#'   \item \code{Historic Return Plot} – ggplot of cumulative portfolio and benchmark returns.
#'   \item \code{Return Series} – xts series of weekly returns for strategy and benchmarks.
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: US sectors
#' tickers_us <- c("IYJ","IYH","IDU","IYF","IYE",
#'                 "IYC","IYK","IYM","IYZ","IYW")
#' results_us <- momentum_strategy(
#'   tickers = tickers_us,
#'   start_date = "2020-01-01",
#'   lookback_period = 8*4,
#'   skip = 1,
#'   top_n = 2,
#'   holding_period = 3*4,
#'   rebalance_interval = 4,
#'   add_benchmark = "SPTM",
#'   Short = FALSE,
#'   include_plot = TRUE
#' )
#' results_us$Summary
#'
#' # Example 2: Global sectors
#' tickers_world <- c(
#'   IXP = "Global Communication Services",
#'   RXI = "Global Consumer Discretionary",
#'   KXI = "Global Consumer Staples",
#'   IXC = "Global Energy",
#'   IXG = "Global Financials",
#'   IXJ = "Global Healthcare",
#'   EXI = "Global Industrials",
#'   MXI = "Global Materials",
#'   IXN = "Global Technology",
#'   JXI = "Global Utilities"
#' )
#'
#' MOM_results_world <- momentum_strategy(
#'   tickers            = names(tickers_world),
#'   start_date         = "2015-01-01",
#'   lookback_period    = 12 * 4,
#'   skip               = 0,
#'   top_n              = 2,
#'   holding_period     = 4,
#'   rebalance_interval = 4,
#'   initial_capital    = 20000,
#'   add_benchmark      = "URTH",
#'   Short              = FALSE,
#'   include_plot       = TRUE
#' )
#' MOM_results_world[["Historic Return Plot"]]
#' }
#'
#' @importFrom quantmod Ad getSymbols
#' @importFrom xts to.period endpoints
#' @importFrom zoo index na.fill as.zoo.data.frame
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal theme
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename arrange desc
#' @importFrom tibble tibble
#'
#' @export
momentum_strategy <- function(tickers,
                              start_date = "2010-01-01",
                              lookback_period = 12 * 4,   # weeks
                              skip = 1,                    # weeks
                              top_n = 2,
                              holding_period = 3 * 4,      # weeks
                              rebalance_interval = 4,      # weeks
                              trade_cost = 50,
                              initial_capital = 100000,
                              add_benchmark = "SPY",
                              Short = TRUE,
                              include_plot = TRUE) {
  if (!exists("ZZ_benchmark", mode = "function")) {
    stop("ZZ_benchmark() not found. Please source your helper function first.")
  }

  periods_per_year <- 52
  start_date <- as.Date(start_date)
  data_start <- start_date - (lookback_period + skip) * 7
  data_end   <- Sys.Date()

  # Helper: convert any xts price series to weekly, labeled as Friday (W-FRI)
  to_weekly_fri <- function(x) {
    ep <- xts::endpoints(x, on = "weeks")
    wk <- xts::period.apply(x, INDEX = ep, FUN = last)
    idx <- as.Date(index(wk))
    wday <- as.POSIXlt(idx)$wday            # 0=Sun ... 5=Fri ... 6=Sat
    delta <- (5 - wday) %% 7                # shift to Friday
    fri_idx <- idx + delta
    xts::xts(coredata(wk), order.by = fri_idx)
  }

  # 1) Download prices (Adjusted Close) and make weekly W-FRI -----------------
  get_one <- function(sym) suppressWarnings(
    quantmod::getSymbols(sym, src = "yahoo", from = data_start, to = data_end, auto.assign = FALSE)
  )
  px_list   <- lapply(tickers, get_one)
  close_wk  <- lapply(px_list, function(x) to_weekly_fri(Ad(x)))
  weekly_px <- do.call(merge, close_wk)
  colnames(weekly_px) <- tickers

  # 2) Filter short histories (>= 90% of max obs) -----------------------------
  counts <- colSums(!is.na(weekly_px))
  threshold <- 0.9 * max(counts, na.rm = TRUE)
  weekly_px <- weekly_px[, counts >= threshold, drop = FALSE]
  message("Remaining ETFs after filtering: ", paste(colnames(weekly_px), collapse = ", "))

  # Weekly simple returns
  weekly_rets <- na.omit(weekly_px / stats::lag(weekly_px, 1) - 1)

  # 3) Industry cumulative returns (ggplot with color + legend ONLY)
  industry_plot <- NULL
  if (NCOL(weekly_rets) > 0) {
    cum_ind <- na.omit(cumprod(1 + weekly_rets))
    df_ind  <- cum_ind |>
      fortify.zoo() |>
      dplyr::rename(date = Index) |>
      tidyr::pivot_longer(-date, names_to = "Symbol", values_to = "Cumulative")

    industry_plot <- ggplot2::ggplot(df_ind, ggplot2::aes(x = date, y = Cumulative, color = Symbol)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::labs(title = "Industry Cum. Returns", x = NULL, y = "Cumulative Return", color = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
  }

  # 4) Momentum scores: pct_change(lookback+skip) shifted by 'skip' -----------
  mom_raw <- (weekly_px / stats::lag(weekly_px, lookback_period + skip)) - 1
  momentum_scores <- stats::lag(mom_raw, k = skip) |> na.omit()

  # Signals (long-only or long/short), rebalance/hold grid --------------------
  idx <- index(momentum_scores)
  signal <- xts::xts(matrix(0, nrow = nrow(momentum_scores), ncol = ncol(momentum_scores)),
                     order.by = idx)
  colnames(signal) <- colnames(momentum_scores)

  n <- NROW(momentum_scores)
  for (i in seq(1, n, by = rebalance_interval)) {
    if (i + holding_period - 1 <= n) {
      sc <- as.numeric(momentum_scores[i, ])
      names(sc) <- colnames(momentum_scores)
      top_assets <- names(sort(sc, decreasing = TRUE))[seq_len(min(top_n, length(sc)))]
      holding_idx <- i:(i + holding_period - 1)
      signal[holding_idx, top_assets] <- signal[holding_idx, top_assets] + 1
      if (isTRUE(Short)) {
        low_assets <- names(sort(sc, decreasing = FALSE))[seq_len(min(top_n, length(sc)))]
        signal[holding_idx, low_assets] <- signal[holding_idx, low_assets] - 1
      }
    }
  }

  # 5) Normalize to equal weights and EXECUTE next week (avoid look-ahead) ----
  rowsums <- rowSums(signal, na.rm = TRUE)
  weights <- signal
  for (i in seq_len(nrow(signal))) {
    denom <- as.numeric(rowsums[i])
    weights[i, ] <- if (denom != 0) signal[i, ] / denom else 0
  }
  weights <- stats::lag(weights, 1)          # execute at next week's open
  weights <- zoo::na.fill(weights, 0)

  # 6) Align to start_date -----------------------------------------------------
  weekly_rets    <- weekly_rets[index(weekly_rets)    >= start_date, ]
  weights        <- weights[index(weights)            >= start_date, ]
  momentum_scores<- momentum_scores[index(momentum_scores)>= start_date, ]
  if (NROW(weights) == 0) stop("No data after start_date; choose an earlier start.")
  message("Strategy starts on ", as.character(first(index(weights))))

  # 7) Portfolio return (align by right join to avoid row drops) --------------
  weights_al <- merge(weights, weekly_rets, join = "right")[, colnames(weights)]
  weights_al[is.na(weights_al)] <- 0
  portfolio_return <- xts::xts(rowSums(weekly_rets * weights_al, na.rm = TRUE),
                               order.by = index(weekly_rets))

  # Turnover / trades (based on executable weights)
  wdiff     <- diff(weights_al)
  turnover  <- na.omit(rowSums(abs(wdiff)))
  trades_mt <- (wdiff != 0) * 1
  trades_mt[is.na(trades_mt)] <- 0
  ep_year <- xts::endpoints(trades_mt, on = "years")
  trades_per_year_vec <- sapply(seq_len(length(ep_year) - 1), function(k) {
    rng <- (ep_year[k] + 1):ep_year[k + 1]
    sum(trades_mt[rng, ], na.rm = TRUE)
  })
  tpy_index <- as.Date(format(index(trades_mt)[ep_year[-1]], "%Y-12-31"))
  trades_per_year <- xts::xts(trades_per_year_vec, order.by = tpy_index)

  # 8) Trading costs -----------------------------------------------------------
  trades_per_week <- xts::xts(rowSums(trades_mt, na.rm = TRUE), order.by = index(trades_mt))
  trades_per_week <- merge(trades_per_week, portfolio_return, join = "right")[, 1]
  gross_cum   <- cumprod(1 + portfolio_return)
  gross_value <- initial_capital * gross_cum
  value_start <- stats::lag(gross_value, 1); if (is.na(value_start[1])) value_start[1] <- initial_capital
  weekly_cost <- trades_per_week * trade_cost
  cost_return <- - (weekly_cost / value_start)
  cost_return <- merge(cost_return, portfolio_return, join = "right")[, 1]
  cost_return[is.na(cost_return)] <- 0
  net_strategy_return <- portfolio_return + cost_return

  # 9) Benchmarks --------------------------------------------------------------
  # 9a) ZZ benchmark from your helper (weekly simple, non-cum), align to port
  zz_df <- ZZ_benchmark(start = data_start, end = data_end,
                        frequency = "weekly", type = "simple", cum = FALSE)
  ZZ_benchmark_returns <- xts::xts(zz_df$ZZ_benchmark, order.by = as.Date(zz_df$date))
  # (Optional) relabel to W-FRI to be super strict
  index(ZZ_benchmark_returns) <- index(to_weekly_fri(ZZ_benchmark_returns))
  ZZ_benchmark_returns <- merge(ZZ_benchmark_returns, portfolio_return, join = "right")[, 1]

  # 9b) Additional benchmark (Adjusted) weekly W-FRI, simple ret
  add_px <- {
    tmp <- quantmod::getSymbols(add_benchmark, src = "yahoo",
                                from = data_start, to = data_end, auto.assign = FALSE)
    to_weekly_fri(Ad(tmp))
  }
  add_ret <- na.omit(add_px / stats::lag(add_px, 1) - 1)
  add_ret <- merge(add_ret, portfolio_return, join = "right")[, 1]

  # 10) Combine & summarize ----------------------------------------------------
  merged_returns <- na.omit(merge(portfolio_return, net_strategy_return,
                                  ZZ_benchmark_returns, add_ret))
  colnames(merged_returns) <- c("Momentum Strategy",
                                "Net Momentum Strategy",
                                "ZZ Benchmark",
                                paste0("Add. Benchmark: ", add_benchmark))

  summarize_returns <- function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    mean_ann <- (1 + m)^periods_per_year - 1
    sd_ann   <- s * sqrt(periods_per_year)
    sharpe   <- if (sd_ann == 0) NA_real_ else mean_ann / sd_ann
    c(`Annualized Avg. Return` = round(mean_ann, 4),
      `Annualized Volatility`  = round(sd_ann, 4),
      `Sharpe Ratio`           = round(sharpe, 4))
  }
  Summary <- t(apply(merged_returns, 2, summarize_returns)) |> as.data.frame()

  # 11) ggplot: Strategy vs Benchmarks cumulative (color + legend ONLY)

  hist_plot <- NULL
  if (isTRUE(include_plot)) {
    mc <- na.omit(cumprod(1 + merged_returns))
    df_mc <- mc |>
      fortify.zoo() |>
      dplyr::rename(date = Index) |>
      tidyr::pivot_longer(-date, names_to = "Series", values_to = "Cumulative")

    hist_plot <- ggplot2::ggplot(df_mc, ggplot2::aes(x = date, y = Cumulative, color = Series)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::labs(title = "Industry Momentum Strategy vs Benchmarks",
                    x = NULL, y = "Cumulative Return", color = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
  }

  # 12) Live weights snapshot (12m lookback, equal-weight top_n) --------------
  ret_lookback <- weekly_px / stats::lag(weekly_px, lookback_period) - 1
  last_ret_series <- as.numeric(last(ret_lookback)); names(last_ret_series) <- colnames(ret_lookback)
  last_ret_series <- last_ret_series[!is.na(last_ret_series)]
  top_today <- names(sort(last_ret_series, decreasing = TRUE))[seq_len(min(top_n, length(last_ret_series)))]
  live_wts <- setNames(rep(0, ncol(weekly_px)), colnames(weekly_px))
  if (length(top_today) > 0) live_wts[top_today] <- 1 / length(top_today)
  weights_today_df <- tibble::tibble(
    Ticker        = colnames(weekly_px),
    `Weight (%)`  = as.numeric(live_wts[colnames(weekly_px)]) * 100,
    `12m Return`  = as.numeric(last_ret_series[colnames(weekly_px)])
  ) |>
    dplyr::arrange(
      dplyr::desc(.data[["Weight (%)"]]),
      dplyr::desc(.data[["12m Return"]])
    )

  list(
    Summary = Summary,
    `Industry Return Plot` = industry_plot,   # ggplot or NULL
    Weights = weights_al,                     # executable weights aligned to returns
    `Weights this Week` = weights_today_df,
    Turnover = mean(turnover, na.rm = TRUE) * 52,  # annualize by weeks
    `Trades per Year` = trades_per_year,
    `Historic Return Plot` = hist_plot,       # ggplot or NULL
    `Return Series` = merged_returns,
    `Trades each year` = trades_per_year
  )
}

