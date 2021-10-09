#' Check for data corrections of X-times magnitude - data.table style
#' @param dat data set
#' @param var variable for which to check for corrections. Default is `"daily_cases"`
#' @param magnitude magnitude of difference that qualifies as a data correction. Default is `10`.
#' @param min_count minimum count of var. Default is `10`.
#' @return Data set with data correction observations removed
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' check_for_data_correction(dat = get_nat_counts, var = "daily_cases", magnitude = 10)
#' }

check_for_data_correction <- function(dat, var = "daily_cases", magnitude = 10,
                                      min_count = 10) {

  v     <- dat[, var, with = FALSE]
  v_lag <- data.table::shift(v)[[1]]
  times <- v / v_lag
  times_loc <- which((times <= magnitude) & (v_lag >= min_count))

  if(length(times_loc) == 0) {

    dat

  } else {

    dat[times_loc]

  }

}
