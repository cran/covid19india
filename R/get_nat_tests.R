#' Pull covid19india national time series test data
#' @param path The URL path for the data. Default: `https://api.covid19india.org/data.json`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @return Pulls the time-series test data directly from covid19india.org.
#' @import data.table
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_nat_tests()
#' }
#'

get_nat_tests <- function(
  path       = "https://data.covid19india.org/csv/latest/tested_numbers_icmr_data.csv",
  raw        = FALSE
) {

  d <- fread(path, showProgress = FALSE)

  if (raw == FALSE) {

    setnames(d, names(d), janitor::make_clean_names(names(d)))

    d <- d[, .(tested_as_of, total_samples_tested)][
      , `:=` (
        place = "India",
        date = as.Date(tested_as_of, "%d/%m/%Y"),
        total_tests = total_samples_tested)
    ][, .SD[total_tests == max(total_tests)], by = "date"][
      order(date), `:=`
      (daily_tests = total_tests - data.table::shift(total_tests),
        ppt        = total_tests / (covid19india::pop[place == "India", population]))
    ][, !c("tested_as_of", "total_samples_tested")][!is.na(place)]

    setkeyv(d, cols = c("place", "date"))
    setcolorder(d, c("place", "date", "daily_tests", "total_tests", "ppt"))

  }



  return(d)

}

