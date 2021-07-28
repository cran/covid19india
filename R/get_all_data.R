#' Pull all covid19india count, test, and vaccine data for states and nation
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @param covind19_name_scheme Variable naming scheme used for development of [`covind19.org`](https://umich-biostatistics.shinyapps.io/covid19/) application
#' @param corr_check Check for data corrections of X-times magnitude. Default is `TRUE`
#' @return Pulls the district-level time-series case, death, and recovered data directly from [`covid19india.org`](https://www.covid19india.org).
#' @import dplyr
#' @import tidyr
#' @importFrom cli cli_alert_warning
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_all_data()
#' }

get_all_data <- function(
  keep_nat             = TRUE,
  covind19_name_scheme = FALSE,
  corr_check           = TRUE
) {

  d <- dplyr::bind_rows(
    covid19india::get_nat_counts(),
    covid19india::get_state_counts()
    ) %>%
    dplyr::left_join(
      dplyr::bind_rows(
        covid19india::get_nat_tests(),
        covid19india::get_state_tests()
      ),
      by = c("place", "date")) %>%
    dplyr::left_join(
      base::suppressMessages(get_r0(., corr_check = corr_check)) %>%
        dplyr::rename(
          r_est   = r,
          r_lower = lower,
          r_upper = upper
        ),
      by = c("place", "date")) %>%
    dplyr::left_join(
      covid19india::pop %>%
        dplyr::distinct(place, .keep_all = TRUE) %>%
        dplyr::select(-population),
      by = "place") %>%
    dplyr::mutate(tpr = daily_cases / daily_tests) %>%
    dplyr::left_join(
      base::suppressMessages(covid19india::get_state_vax()),
        by = c("place", "date")
    )

  if (keep_nat == FALSE) {

    d %>% dplyr::filter(place != "India")

  }

  if (covind19_name_scheme == TRUE) {

    cli::cli_alert_warning("{covid19india} variable naming scheme is encouraged (`covind19_naming_scheme == FALSE`)")

    d <- d %>%
      dplyr::rename(
        cases            = daily_cases,
        recovered        = daily_recovered,
        deaths           = daily_deaths,
        total_vacc       = total_doses,
        pct_at_least_one = pct_one_doses,
        pct_second       = pct_two_doses,
        daily_vax_dose   = daily_doses
      )

  }

  return(d)

}
