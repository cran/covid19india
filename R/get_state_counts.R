#' Pull covid19india state
#' @param path The URL path for the data. Default: `https://api.covid19india.org/csv/latest/state_wise_daily.csv`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import data.table
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_state_counts()
#' }

get_state_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/state_wise_daily.csv",
  raw        = FALSE,
  keep_nat   = FALSE,
  corr_check = FALSE
) {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      d <- d[, !c("Date")][, DH := DD + DN][, !c("DD", "DN")]
      setnames(d, names(d), janitor::make_clean_names(names(d)))
      setnames(d, "date_ymd", "date")

      d <- data.table::melt(d, id.vars = c("date", "status"), variable.name = "abbrev")
      d <- data.table::dcast(d, date + abbrev ~ status)
      d <- d[abbrev != "un"]

      setnames(d,
               c("Confirmed", "Deceased", "Recovered"),
               c("daily_cases", "daily_deaths", "daily_recovered"))

      d <- d[daily_cases >= 0][order(date),
                               `:=` (
                                 total_cases     = cumsum(daily_cases),
                                 total_deaths    = cumsum(daily_deaths),
                                 total_recovered = cumsum(daily_recovered)
                               ),
                               by = abbrev][,
                                            date := as.Date(date)]

      d <- data.table::merge.data.table(d, covid19india::pop[, !c("population")], by = "abbrev", all.x = TRUE)[, !c("abbrev")]

      setkeyv(d, cols = c("place", "date"))
      setcolorder(d, neworder = c("place", "date", "daily_cases", "daily_recovered", "daily_deaths", "total_cases", "total_recovered", "total_deaths"))

    }

  if (keep_nat == FALSE) {
    if (raw == FALSE) {
      d <- d[place != "India"]
    }
    if (raw == TRUE) {
      d <- d[, !c("TT")]
    }

  }

  if (corr_check == TRUE) {

    if (raw == TRUE) {

      stop("`raw` must be FALSE to use `corr_check = TRUE` argument")

    } else {

      d <- data.table::rbindlist(
        lapply(d[, unique(place)],
               function(x) covid19india::check_for_data_correction(d[place == x]))
      )

    }

  }

  return(d)

}
