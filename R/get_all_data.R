#' Pull all covid19india count, test, and vaccine data for states and nation
#' @param keep_nat Keep the national data as well. Default is `FALSE`
#' @param covind19_name_scheme Variable naming scheme used for development of [`covind19.org`](https://umich-biostatistics.shinyapps.io/covid19/) application
#' @param corr_check Check for data corrections of X-times magnitude. Default is `TRUE`
#' @return Pulls the district-level time-series case, death, and recovered data directly from [`covid19india.org`](https://www.covid19india.org).
#' @import data.table
#' @importFrom cli cli_alert_warning
#' @importFrom janitor clean_names
#' @importFrom utils head
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

  d <- data.table::merge.data.table(
    data.table::rbindlist(list(
      get_nat_counts(),
      get_state_counts()
    ), fill = TRUE),
    data.table::rbindlist(list(
      get_nat_tests(),
      get_state_tests()
    ), fill = TRUE),
    by    = c("place", "date"),
    all.x = TRUE
  )

  d <- data.table::merge.data.table(
    d,
    get_r0(dat = d, corr_check = corr_check)[, .(place, date, r_est = r, r_lower = lower, r_upper = upper)],
    by    = c("place", "date"),
    all.x = TRUE
  )

  d <- data.table::merge.data.table(
    d,
    unique(covid19india::pop[, .(place, abbrev)][, utils::head(.SD, 1), by = "place"]),
    by    = "place",
    all.x = TRUE
  )

  d <- data.table::merge.data.table(
    d[, tpr := daily_cases / daily_tests],
    base::suppressMessages(get_state_vax()),
    by = c("place", "date"),
    all.x = TRUE
  )

  if (keep_nat == FALSE) {

    d <- d[place != "India"]

  }

  if (covind19_name_scheme == TRUE) {

    cli::cli_alert_warning("{covid19india} variable naming scheme is encouraged (`covind19_naming_scheme == FALSE`)")

    setnames(d,
             old = c("daily_cases", "daily_recovered", "daily_deaths", "total_doses", "pct_one_doses",
                     "pct_two_doses", "daily_doses"),
             new = c("cases", "recovered", "deaths", "total_vacc", "pct_at_least_one", "pct_second", "daily_vax_dose"))
  }

  return(d)

}
