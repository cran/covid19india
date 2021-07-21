#' Helper function for calculating R_0
#' @param dat Input dataset
#' @return Pulls the time-series state-level testing data directly from covid19india.org.
#' @keywords internal
#' @import dplyr
#' @import EpiEstim
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom tidyselect everything
#' @examples
#' \dontrun{
#' get_nat_counts() %>%
#'   estR0_out()
#' }
#'

estR0_out <- function(dat) {

  t_start <- seq(2, nrow(dat) - 4)
  t_end   <- t_start + 4

  res <- EpiEstim::estimate_R(
    incid = dat$daily_cases,
    method = "parametric_si",
    config = EpiEstim::make_config(list(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = 46342))
  )

  tibble::tibble(
    date_num = res$dates
  ) %>% dplyr::left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    tibble::add_column(date = dat$date) %>%
    dplyr::select(-c(date_num, t_start)) %>%
    dplyr::select(date, tidyselect::everything())

}

#' Calculate r0
#' @param dat Input dataset. Expects \code{daily_cases}, \code{total_cases}, and \code{place} columns
#' @param place_var Variable corresponding to the place. If data corresponds to single place, this variable is arbitrary. Default `place_var` is `place`
#' @param daily_var Variable corresponding to the daily (incidence) case count data. Default is `daily_cases`
#' @param total_var Variable corresponding to the total (cumulative) case count data. Default is `total_cases`
#' @param daily_filter Threshold for minimum daily cases. Default = `0`.
#' @param total_filter Threshold for minimum total cases reported to date. Default = `50`.
#' @param min_date Threshold for earliest date to report R_0. Default = `"2020-03-23"`.
#' @return Pulls the time-series state-level testing data directly from covid19india.org. Expects columns named `place`, `daily_cases`, and `total_cases`. Can specify corresponding variables through other arguments.
#' @import dplyr
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_nat_counts() %>% get_r0()
#' }
#'

get_r0 <- function(
  dat,
  place_var    = place,
  daily_var    = daily_cases,
  total_var    = total_cases,
  daily_filter = 0,
  total_filter = 50,
  min_date     = "2020-03-23"
  ) {

  if ((deparse(substitute(place_var)) %in% names(dat)) == FALSE) {
    stop(paste0("Looking for ", deparse(substitute(place_var)), " in data, but not found. ",
                "Check if `place_var` argument is correctly specified"))
  }

  if ((deparse(substitute(daily_var)) %in% names(dat)) == FALSE) {
    stop(paste0("Looking for ", deparse(substitute(daily_var)), " in data, but not found. ",
                "Check if `daily_var` argument is correctly specified"))
  }

  if ((deparse(substitute(total_var)) %in% names(dat)) == FALSE) {
    stop(paste0("Looking for ", deparse(substitute(total_var)), " in data, but not found. ",
                "Check if `total_var` argument is correctly specified"))
  }

  tmp_dat <- dat %>%
    dplyr::filter({{ daily_var }} > daily_filter & {{ total_var }} >= total_filter) %>%
    dplyr::group_by({{ place_var }}) %>%
    dplyr::mutate(
      ns = n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ns >=7) %>%
    dplyr::rename(daily_cases = {{ daily_var }}, place = {{ place_var }})

  tmp_est <- suppressWarnings(tmp_dat %>%
    dplyr::select(date, daily_cases, place) %>%
    tidyr::nest(data = c(-place)) %>%
    dplyr::mutate(
      estR0 = purrr::map(data, ~estR0_out(dat = .x))
    ) %>%
    tidyr::unnest(estR0) %>%
    dplyr::select(-data) %>%
    dplyr::filter(date >= min_date) %>%
    tidyr::drop_na())

  return(tmp_est)

}
