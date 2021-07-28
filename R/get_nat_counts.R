#' Pull covid19india national time series data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/case_time_series.csv
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_nat_counts()
#' }
#'

get_nat_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/case_time_series.csv",
  raw        = FALSE,
  corr_check = FALSE
) {

  d <- readr::read_csv(path,
                       col_types = readr::cols())

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::select(-date) %>%
      dplyr::rename(
        date         = date_ymd,
        daily_cases  = daily_confirmed,
        total_cases  = total_confirmed,
        daily_deaths = daily_deceased,
        total_deaths = total_deceased
        ) %>%
      dplyr::mutate(
        place = "India"
      ) %>%
      dplyr::select(place, date, daily_cases, daily_recovered, daily_deaths, everything())

  }

  if (corr_check == TRUE) {

    if (raw == TRUE) {
      stop("`raw` must be FALSE to use `corr_check = TRUE` argument")
    } else {

    d <- d %>%
      covid19india::check_for_data_correction(var = "daily_cases")

    }
  }

  return(d)

}
