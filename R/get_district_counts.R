#' Pull covid19india district-level data
#' @param path The URL path for the data. Default: `https://api.covid19india.org/csv/latest/districts.csv`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @return Pulls the district-level time-series case, death, and recovered data directly from covid19india.org.
#' @import dplyr
#' @import tidyr
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#' get_district_counts()
#' }

get_district_counts <- function(
  path       = "https://api.covid19india.org/csv/latest/districts.csv",
  raw        = FALSE
) {

  d <- readr::read_csv(path,
                       col_types = readr::cols())

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::rename(
        total_cases     = confirmed,
        total_recovered = recovered,
        total_deaths    = deceased
      ) %>%
      dplyr::filter(dplyr::select(., where(is.numeric)) >= 0) %>%
      dplyr::group_by(state, district) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        daily_cases     = total_cases - dplyr::lag(total_cases),
        daily_recovered = total_recovered - dplyr::lag(total_recovered),
        daily_deaths    = total_deaths - dplyr::lag(total_deaths)
      ) %>%
      tidyr::drop_na(daily_cases, daily_recovered, daily_deaths) %>%
      dplyr::ungroup() %>%
      dplyr::select(state, district, date,
                    daily_cases, daily_recovered, daily_deaths,
                    total_cases, total_recovered, total_deaths)

  }

  return(d)

}
