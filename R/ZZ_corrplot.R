#' Download Portfolio Data and create Correlation Analysis
#'
#' Downloads price data for all portfolio holdings from Yahoo Finance and conducts
#' a correlation analysis with the help of corrplot.
#' It plots the correlation plot and saves the correlation matrix.
#'
#' @param df Dataframe with the return series including date column (Optional).
#' @param frequency Defines the frequency of the return series ("daily", "weekly", "monthly").
#' @param type Defines if simple or log returns should be returned.
#'
#' @return A corrplot with correlation matrix visualised.
#' @examples
#' \dontrun{
#'   # Example:
#'   ZZ_corrplot(
#'     df = strat_ret,
#'     frequency = "weekly",
#'     type = "simple",
#'   )
#' }
#' @importFrom quantmod Ad getSymbols
#' @importFrom xts to.period
#' @importFrom zoo index coredata
#' @importFrom corrplot corrplot
#' @export

ZZ_corrplot <- function(df = NULL , frequency = c("daily", "weekly", "monthly"), type = c("simple", "log")){

  # Inputs
  frequency <- match.arg(frequency)
  type <- match.arg(type)
  start <- as.Date("2020-01-01")
  end <- Sys.Date()

  # ZZ Portfolio Investments (needs constant updates/ later connect to Bloomberg)

  tickers <- c(
    "SWICHA.SW",    # MSCI Switzerland ETF CHF
    "IXG",          # iShares Global Financials ETF
    "IXP",          # iShares Global Comm Services ETF
    "LKOR.DE",      # Amundi MSCI Korea UCITS ETF Acc
    "IGLE.AS",      # iShares Global Govt Bond UCITS ETF EUR Hedged
    "IEML.L",       # iShares J.P. Morgan EM Local Govt Bond UCITS ETF USD
    "IEGA.AS",      # iShares Core € Govt Bond UCITS ETF EUR
    "ETFBW20TR.WA", # Beta ETF WIG20TR
    "IEMD.L",       # iShares Edge MSCI Europe Momentum Factor UCITS ETF EUR
    "BDX.WA"        # Budimex S.A.
  )

  # Download Adjusted Close prices
  options(getSymbols.warning4.0 = FALSE)

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

  if (type == "log") {
    rets_xts <- diff(log(prices))
  } else {
    rets_xts <- prices / lag(prices) - 1
  }

  rets_xts <- rets_xts[complete.cases(rets_xts), ]

  # Build a data.frame with date column

  portfolio_df <- data.frame(date = index(rets_xts), coredata(rets_xts), check.names = FALSE)

  if (!is.null(df)) {
    if (!("date" %in% names(df))) stop("`df` must contain a 'date' column.")
    if (!inherits(df$date, "Date")) df$date <- as.Date(df$date)
    # Keep only numeric cols (besides date)
    keep <- vapply(df, is.numeric, logical(1)); keep["date"] <- TRUE
    df <- df[, keep, drop = FALSE]
    # Avoid duplicate names
    dup <- intersect(names(df), names(portfolio_df))
    dup <- setdiff(dup, "date")
    if (length(dup)) names(df)[match(dup, names(df))] <- paste0(dup, "_df")
    merged_df <- merge(portfolio_df, df, by = "date", all = FALSE)
  } else {
    merged_df <- portfolio_df
  }

  # Create Correlation Matrix

  num_cols <- vapply(merged_df, is.numeric, logical(1))
  num_mat  <- as.matrix(merged_df[, num_cols, drop = FALSE])
  cor_mat <- cor(num_mat, use = "pairwise.complete.obs")


  # Create Corrplot

  corrplot::corrplot(
    cor_mat,
    method = "color",
    type   = "upper",
    order  = "hclust",
    diag   = FALSE,
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.7,              # ↓ label font size
    cl.cex = 0.7,              # ↓ legend font size (optional)
    addCoef.col   = "black",   # print numbers
    number.cex    = 0.6,       # number size
    number.digits = 2,         # decimals
    col = colorRampPalette(c("blue", "white", "red"))(200)
  )

  # It will print the correlation plot and save the correlation matrix
  invisible(cor_mat)

}




