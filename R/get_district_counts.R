#' Pull covid19india district-level data
#' @param path The URL path for the data. Default: `https://api.covid19india.org/csv/latest/districts.csv`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @return Pulls the district-level time-series case, death, and recovered data directly from covid19india.org.
#' @import data.table
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_district_counts()
#' }

get_district_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/districts.csv",
  raw        = FALSE
) {

  d <- data.table::fread(path, showProgress = FALSE)

  if (raw == FALSE) {

    setnames(d, names(d), janitor::make_clean_names(names(d)))
    setnames(d, c("confirmed", "recovered", "deceased"), c("total_cases", "total_recovered", "total_deaths"))

    d <- data.table::melt(d[, !c("other")], id.vars = c("date", "state", "district"))[value >= 0]

    d <- data.table::dcast.data.table(d, date + state + district ~ variable)[order(date)][
      , `:=` (
        daily_cases     = total_cases - data.table::shift(total_cases),
        daily_recovered = total_recovered - data.table::shift(total_recovered),
        daily_deaths    = total_deaths - data.table::shift(total_deaths)
      ), by = c("state", "district")
    ][!is.na(daily_cases) & !is.na(daily_recovered) & !is.na(daily_deaths)][
      , date := as.Date(date)
    ][
      , .(state, district, date,
          daily_cases, daily_recovered, daily_deaths,
          total_cases, total_recovered, total_deaths)
    ]

    d <- na.omit(d, c("daily_cases", "daily_recovered", "daily_deaths"))

    setkeyv(d, cols = c("state", "district", "date"))

  }

  return(d)

}
