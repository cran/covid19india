#' Pull covid19india national time series data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/case_time_series.csv
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import data.table
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_nat_counts_dt()
#' }
#'

get_nat_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/case_time_series.csv",
  raw        = FALSE,
  corr_check = FALSE
) {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      d <- d[, !c("Date")]
      setnames(d, names(d), janitor::make_clean_names(names(d)))
      setnames(d,
               c("date_ymd", "daily_confirmed", "total_confirmed", "daily_deceased", "total_deceased"),
               c("date", "daily_cases", "total_cases", "daily_deaths", "total_deaths"))

      d <- d[, `:=` (place = "India", date = as.Date(date))]

      setcolorder(d,
                  neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths", "total_cases", "total_recovered", "total_deaths"))

      setkeyv(d, cols = c("place", "date"))

    }

    if (corr_check == TRUE) {

      if (raw == TRUE) {
        stop("`raw` must be FALSE to use `corr_check = TRUE` argument")
      } else {
        d <- covid19india::check_for_data_correction(dat = d, var = "daily_cases")
      }
    }

  return(d)

}
