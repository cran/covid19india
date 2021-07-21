#' Pull covid19india state-level testing data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @return Pulls the time-series state-level testing data directly from covid19india.org.
#' @import dplyr
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#' get_state_counts()
#' }

get_state_tests <- function(
  path       = "https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv",
  raw        = FALSE
) {

  d <- suppressWarnings(readr::read_csv(path,
                                        col_types = readr::cols()))

  if (raw == FALSE) {

    d <- d %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        date = as.Date(updated_on, "%d/%m/%Y")
      ) %>%
      dplyr::select(date, place = state, total_tests = total_tested) %>%
      distinct() %>%
      dplyr::group_by(place) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        daily_tests = total_tests - dplyr::lag(total_tests)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(place, date, daily_tests, total_tests) %>%
      dplyr::left_join(pop %>% dplyr::select(-abbrev) %>% dplyr::distinct(), by = "place") %>%
      dplyr::mutate(ppt = total_tests / population) %>%
      dplyr::select(-population)

  }

  return(d)

}
