#' Data Cleaning of data frames
#'
#' Delete all columns and or rows of a data frame with NAs up to a specified threshold.
#' Optionally, missing data points can be linearly interpolated.
#'
#' @param df Dataframe with the data.
#' @param row_thresh Defines the threshold (in proportion) for deleting a row (base is 0.5).
#' @param col_thresh Defines the threshold (in proportion) for deleting a column (base is 0.5).
#' @param interpolate Optionally, linearly interpolate missing data points
#' @param protect Character vector of column names to keep untouched (preset to "date").
#'
#' @return Returns a cleaned data frame for further analysis
#' @examples
#' \dontrun{
#'   # Example:
#'   ZZ_remove_na(
#'     df = strat_ret,
#'     row_thresh = 0.5,
#'     col_thresh = 0.5,
#'     interpolate = FALSE,
#'     protect = "date")
#' }
#' @export

ZZ_remove_na <- function(df, row_thresh = 0.5, col_thresh = 0.5,
                         interpolate = FALSE, protect = "date") {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Convert percentage to proportion if needed
  if (row_thresh > 1) row_thresh <- row_thresh / 100
  if (col_thresh > 1) col_thresh <- col_thresh / 100

  # Step 1: Drop rows with too many NAs
  row_na_prop <- rowMeans(is.na(df))
  df <- df[row_na_prop <= row_thresh, , drop = FALSE]
  if (!nrow(df)) return(df)

  # Step 2: Drop columns with too many NAs, but always keep protect
  col_na_prop <- colMeans(is.na(df))
  keep_cols <- col_na_prop <= col_thresh | names(df) %in% protect
  df <- df[, keep_cols, drop = FALSE]
  if (!ncol(df)) return(df)

  # Step 3: Interpolate missing values (optional, only numeric + not protected)
  if (interpolate) {
    is_numeric <- sapply(df, is.numeric)
    is_numeric[names(df) %in% protect] <- FALSE

    df[is_numeric] <- lapply(df[is_numeric], function(x) {
      na_idx <- is.na(x)
      if (all(na_idx) || !any(!na_idx)) return(x)  # skip all-NA
      approx(x = which(!na_idx),
             y = x[!na_idx],
             xout = seq_along(x),
             rule = 2)$y
    })
  }

  return(df)
}
