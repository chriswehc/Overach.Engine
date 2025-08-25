#' Download Portfolio Data and create Correlation Analysis
#'
#' Downloads price data for all portfolio holdings from Yahoo Finance and conducts
#' a correlation analysis with the help of corrplot.
#' It plots the correlation plot and saves the correlation matrix.
#'
#' @param df Dataframe with the return series including date column (Optional).
#' @param frequency Defines the frequency of the return series ("daily", "weekly", "monthly").
#' @param type Defines if simple or log returns should be returned.
#' @param fuzzy logical; if TRUE, nearest-date join is used for `df`.
#' @param max_days integer; maximum day distance for nearest-date join.
#'
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
#' @import dplyr
#' @import fuzzyjoin
#' @export

ZZ_corrplot <- function(df = NULL,
                        frequency = c("daily", "weekly", "monthly"),
                        type = c("simple", "log"),
                        fuzzy = TRUE,
                        max_days = 5) {

  frequency <- match.arg(frequency)
  type      <- match.arg(type)
  start <- as.Date("2020-01-01")
  end   <- Sys.Date()

  tickers <- c(
    "SWICHA.SW","IXG","IXP","LKOR.DE","IGLE.AS",
    "IEML.L","IEGA.AS","ETFBW20TR.WA","IEMD.L","BDX.WA"
  )

  # --- Download Adjusted Close prices ---
  options(getSymbols.warning4.0 = FALSE)
  close_list <- lapply(tickers, function(tkr) {
    quantmod::Ad(quantmod::getSymbols(tkr, from = start, to = end, src = "yahoo", auto.assign = FALSE))
  })
  close_prices <- do.call(merge, close_list)
  colnames(close_prices) <- tickers

  if (frequency == "weekly") {
    prices <- xts::to.period(close_prices, period = "weeks",  indexAt = "endof", drop.time = TRUE, OHLC = FALSE)
  } else if (frequency == "monthly") {
    prices <- xts::to.period(close_prices, period = "months", indexAt = "endof", drop.time = TRUE, OHLC = FALSE)
  } else {
    prices <- close_prices
  }
  colnames(prices) <- tickers

  prices_df <- data.frame(date = zoo::index(prices), zoo::coredata(prices), check.names = FALSE)

  # Optional: your NA-cleaner
  prices_df <- ZZ_remove_na(prices_df, interpolate = TRUE)

  # --- Returns ---
  if (type == "log") {
    portfolio_df <- dplyr::mutate(prices_df, dplyr::across(-date, ~ log(.x) - dplyr::lag(log(.x))))
  } else {
    portfolio_df <- dplyr::mutate(prices_df, dplyr::across(-date, ~ .x / dplyr::lag(.x) - 1))
  }
  # drop first NA row (from lag)
  portfolio_df <- portfolio_df[stats::complete.cases(portfolio_df$date) & !is.na(portfolio_df[[2]]), , drop = FALSE]
  # ensure Date type
  portfolio_df$date <- as.Date(portfolio_df$date)

  # --- Optional merge with external df (exact or fuzzy by nearest date) ---
  if (!is.null(df)) {
    if (!("date" %in% names(df))) stop("`df` must contain a 'date' column.")
    df$date <- as.Date(df$date)

    # keep only numeric cols + date
    keep <- vapply(df, is.numeric, logical(1)); keep["date"] <- TRUE
    df <- df[, keep, drop = FALSE]

    # avoid duplicate names (except date)
    dup <- setdiff(intersect(names(df), names(portfolio_df)), "date")
    if (length(dup)) names(df)[match(dup, names(df))] <- paste0(dup, "_df")

    if (!fuzzy) {
      merged_df <- dplyr::inner_join(portfolio_df, df, by = "date")
    } else {
      merged_df <- fuzzyjoin::difference_left_join(
        portfolio_df, df,
        by = "date",
        max_dist = as.difftime(max_days, units = "days"),
        distance_col = "date_diff"
      ) |>
        dplyr::mutate(
          date_diff = as.numeric(date_diff, units = "days"),
          date_diff = dplyr::if_else(is.na(date_diff), Inf, date_diff)
        ) |>
        dplyr::group_by(date.x) |>
        dplyr::slice_min(date_diff, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::rename(date = date.x) |>
        dplyr::select(-date.y, -date_diff)
    }
  } else {
    merged_df <- portfolio_df
  }

  # --- Correlation matrix ---
  num_cols <- vapply(merged_df, is.numeric, logical(1))
  num_mat  <- as.matrix(merged_df[, num_cols, drop = FALSE])

  if (ncol(num_mat) < 2L) stop("Not enough numeric columns to compute correlations.")
  # drop constant columns to avoid NA correlations
  sd_vec <- apply(num_mat, 2, stats::sd, na.rm = TRUE)
  num_mat <- num_mat[, sd_vec > 0, drop = FALSE]

  cor_mat <- stats::cor(num_mat, use = "pairwise.complete.obs")

  # --- Corrplot ---
  corrplot::corrplot(
    cor_mat,
    method = "color",
    type   = "upper",
    order  = "hclust",
    diag   = FALSE,
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.7,
    cl.cex = 0.7,
    addCoef.col   = "black",
    number.cex    = 0.6,
    number.digits = 2,
    col = grDevices::colorRampPalette(c("blue", "white", "red"))(200)
  )

  invisible(cor_mat)
}



