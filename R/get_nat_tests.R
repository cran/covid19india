#' Pull covid19india national time series test data
#' @param path The URL path for the data. Default: `https://api.covid19india.org/data.json`
#' @param raw Pull raw unaltered data. Default is `FALSE`
#' @return Pulls the time-series test data directly from covid19india.org.
#' @import tidyverse
#' @import httr
#' @importFrom janitor clean_names
#' @importFrom stringr word
#' @export
#' @examples
#' \dontrun{
#' get_nat_tests()
#' }
#'

get_nat_tests <- function(
  path       = "https://api.covid19india.org/data.json",
  raw        = FALSE
) {

  request  <- httr::GET(path)
  json     <- httr::content(request)
  d        <- purrr::map_dfr(json[['tested']], ~ .x)

  if (raw == FALSE) {

    d <- d %>%
      dplyr::select(
        Cases = totalpositivecases,
        Tests = totalsamplestested,
        Date  = testedasof
      ) %>%
      dplyr::mutate(
        Date    = as.Date(stringr::word(Date, 1), format = "%d/%m/%Y"),
        Cases   = as.numeric(gsub(",", "", Cases)),
        Tests   = as.numeric(gsub(",", "", Tests)),
        Country = "India"
      ) %>%
      janitor::clean_names() %>%
      dplyr::select(date, place = country, total_tests = tests) %>%
      dplyr::group_by(date) %>%
      dplyr::filter(total_tests == max(total_tests)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        daily_tests = total_tests - dplyr::lag(total_tests),
        ppt         = total_tests / (pop %>% dplyr::filter(place == "India") %>% dplyr::pull(population))
      )

  }

  return(d)

}

