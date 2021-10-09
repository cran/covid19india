#' Pull covid19india state-level testing data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @return Pulls the time-series state-level testing data directly from covid19india.org.
#' @import data.table
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_state_tests()
#' }

get_state_tests <- function(
  path       = "https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv",
  raw        = FALSE
) {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      setnames(d, names(d), janitor::make_clean_names(names(d)))

      d <- unique(d[, date := as.Date(updated_on, "%d/%m/%Y")][, .(date, place = state, total_tests = total_tested)])

      d <- d[order(date), daily_tests := total_tests - data.table::shift(total_tests), by = place][
        , .(place, date, daily_tests, total_tests)
      ]

      d <- data.table::merge.data.table(d, unique(covid19india::pop[, !c("abbrev")]), all.x = TRUE, by = "place")[
        , ppt := total_tests / population
      ][, !c("population")][!(is.na(daily_tests) & is.na(total_tests) & is.na(ppt))]

      setkeyv(d, cols = c("place", "date"))
      setcolorder(d, c("place", "date", "daily_tests", "total_tests", "ppt"))

    }

  return(d)

}
