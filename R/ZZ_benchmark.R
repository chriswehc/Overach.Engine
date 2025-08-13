#' Create ZZ Benchmark Return Series
#'
#' Downloads price data from the MSCI World and MSCI Emerging Markets ETF from Yahoo Finance and applies
#' a predefined 60/40 weighting to calculate a weighted portfolio return.
#'
#' @param start Start date (Date or character in YYYY-MM-DD format).
#' @param end End date (Date or character in YYYY-MM-DD format).
#' @param frequency Defines the frequency of the return series ("daily", "weekly", "monthly").
#' @param type Defines if simple or log returns should be returned.
#' @param cum Activates cumulative returns.
#' @param label_month_end Activate that instead of the last available trading day, the end-of-month date is returned.
#'
#' @return A data frame with columns: \code{date} and \code{weighted_return}.
#' @examples
#' \dontrun{
#'   # Example:
#'   ZZ_benchmark(
#'     start = "2020-01-01",
#'     end = Sys.Date(),
#'     frequency = "weekly",
#'     type = "simple",
#'     cum = TRUE,
#'     label_month_end = FALSE
#'   )
#' }
#' @importFrom quantmod Ad getSymbols
#' @importFrom xts to.period
#' @importFrom zoo index as.yearmon
#' @export
ZZ_benchmark <- function(start, end, frequency = c("daily", "weekly", "monthly"), type = c("simple", "log"), cum = FALSE, label_month_end = FALSE) {

  frequency <- match.arg(frequency)
  tickers <- c("URTH", "EEM")
  weights <- c(0.6, 0.4)
  type <- match.arg(type)

  # Download Adjusted Close prices
  close_list <- lapply(tickers, function(tkr) {
    Ad(getSymbols(tkr, from = start, to = end, src = "yahoo",
                                      auto.assign = FALSE))
  })
  close_prices <- do.call(merge, close_list)
  colnames(close_prices) <- tickers

  if (frequency == "weekly") {
    prices <- to.period(close_prices, period = "weeks",
                             indexAt = "endof", drop.time = TRUE, OHLC = FALSE)
    colnames(prices) <- tickers
  } else if (frequency == "monthly") {
    prices <- to.period(close_prices, period = "months",
                             indexAt = "endof", drop.time = TRUE, OHLC = FALSE)
    colnames(prices) <- tickers
  } else {
    prices <- close_prices
  }
  # Calculate returns
  returns <- prices / lag(prices) - 1

  # Weighted returns
  weighted_returns <- rowSums(returns * matrix(rep(weights, each = NROW(returns)),
                                               ncol = 2, byrow = FALSE),na.rm = TRUE)

  if(type == "log"){
    if(cum == TRUE) weighted_return = cumsum(log(1 + as.numeric(weighted_returns)))
    else weighted_return = log(1 + as.numeric(weighted_returns))
  }
  else{
    if(cum == TRUE) weighted_return = cumprod(1+ as.numeric(weighted_returns)) - 1
    else weighted_return = as.numeric(weighted_returns)
  }
  # Adjust dates if necessary
  out_dates <- index(prices)

  if(frequency == "monthly" && isTRUE(label_month_end)){
    out_dates <-  zoo::as.Date.yearmon(as.yearmon(out_dates), frac = 1)
  }

  # Return as data.frame
  data.frame(
    date = out_dates,
    weighted_return = weighted_return
  )
}


