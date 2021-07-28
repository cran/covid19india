#' Pull covid19india state
#' @param path The URL path for the data. Default: `https://api.covid19india.org/csv/latest/state_wise_daily.csv`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @param corr_check Check for data correction. Default is `FALSE`
#' @return Pulls the time-series case, death, and recovered data directly from covid19india.org.
#' @import dplyr
#' @import tidyr
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
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

  d <- readr::read_csv(path,
                       col_types = readr::cols())

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::select(-date) %>%
      dplyr::rename(
        date = date_ymd
      ) %>%
      dplyr::mutate(
        dh = dd + dn
      ) %>%
      dplyr::select(-c(dd, dn)) %>%
      tidyr::pivot_longer(
        names_to  = "abbrev",
        values_to = "count",
        -c(date, status)
      ) %>%
      tidyr::pivot_wider(
        names_from  = "status",
        values_from = "count",
        id_cols     = c(date, abbrev)
      ) %>%
      dplyr::rename(
        daily_cases     = Confirmed,
        daily_recovered = Recovered,
        daily_deaths    = Deceased
      ) %>%
      dplyr::filter(abbrev != "un") %>%
      dplyr::filter(dplyr::select(., where(is.numeric)) >= 0) %>%
      dplyr::group_by(abbrev) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        total_cases     = cumsum(daily_cases),
        total_recovered = cumsum(daily_recovered),
        total_deaths    = cumsum(daily_deaths)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(covid19india::pop %>% dplyr::select(-population), by = "abbrev") %>%
      dplyr::select(-abbrev) %>%
      dplyr::select(place, dplyr::everything())

  }

  if (keep_nat == FALSE) {
    if (raw == FALSE) {
      d <- d %>%
        dplyr::filter(!(place %in% c("India")))
    }
    if (raw == TRUE) {
      d <- d %>%
        dplyr::select(-TT)
    }

  }

  if (corr_check == TRUE) {

    if (raw == TRUE) {

      stop("`raw` must be FALSE to use `corr_check = TRUE` argument")

    } else {

      d <- d %>%
        tidyr::nest(data = !place) %>%
        dplyr::mutate(
          data = purrr::map(data, ~covid19india::check_for_data_correction(dat = .x))
        ) %>%
        tidyr::unnest(data)

    }

  }

  return(d)

}
