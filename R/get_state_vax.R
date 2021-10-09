#' Pull covid19india state-level vaccine data
#' @param path The URL path for the data. Default: https://api.covid19india.org/csv/latest/vaccine_doses_statewise_v2.csv
#' @param raw Pull raw unaltered data. Default is FALSE
#' @param keep_nat Keep national level data? Default is TRUE
#' @return Pulls the time-series state-level vaccine data directly from covid19india.org.
#' @import data.table
#' @importFrom cli cli_alert_info
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_state_vax()
#' }

get_state_vax <- function(
  path       = "https://api.covid19india.org/csv/latest/vaccine_doses_statewise_v2.csv",
  raw        = FALSE,
  keep_nat   = TRUE
) {

    d <- data.table::fread(path, showProgress = FALSE)

    if (raw == FALSE) {

      setnames(d, names(d), janitor::make_clean_names(names(d)))

      d <- d[, date := as.Date(vaccinated_as_of, "%d/%m/%Y")][
        , .(place       = state, date,
            first_dose  = first_dose_administered,
            second_dose = second_dose_administered,
            total_doses = total_doses_administered)
      ][place == "Total", place := "India"]

      d <- data.table::merge.data.table(d, unique(covid19india::pop[, !c("abbrev")]), by = "place", all.x = TRUE)[
        , `:=` (pct_one_dose  = round(first_dose * 100 / population, 4),
                pct_two_doses = round(second_dose * 100/ population, 4))
      ][, !c("population")][
        order(date), daily_doses := total_doses - data.table::shift(total_doses), by = "place"
      ]

      setcolorder(d, c("place", "date", "first_dose", "second_dose", "total_doses",
                       "pct_one_dose", "pct_two_doses", "daily_doses"))
      setkeyv(d, cols = c("place", "date"))

    }

  if (keep_nat == FALSE & raw == FALSE) {
    d <- d[place != "India"]
  }

  return(d)

}
