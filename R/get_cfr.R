#' Helper function for calculating case-fatality rate
#' @param C Cases vector
#' @param D Deaths vector
#' @return A case-fatality rate estimate and 95% confidence limits
#' @keywords internal
#' @examples
#' \dontrun{
#' national_data <- get_nat_counts()
#' CFR(C = national_data$total_cases, D = national_data$total_deaths)
#' }
#'
#'
CFR <- function(C,D) {
  cfr       <- D / C
  cfr_logit <- log(cfr) - log(1 - cfr)
  sd_logit  <- sqrt(C / (D * (C - D)))

  lower_logit <- cfr_logit - stats::qnorm(0.975) * sd_logit
  upper_logit <- cfr_logit + stats::qnorm(0.975) * sd_logit

  upper <- exp(upper_logit) / (1 + exp(upper_logit))
  lower <- exp(lower_logit) / (1 + exp(lower_logit))

  return(c(cfr, upper, lower))
}

#' Calculate case_fataility rate
#' @param dat Input dataset. Expects `total_cases` and `total_deaths` variables
#' @return Calculates a case-fatality rate estimate and corresponding 95% confidence interval
#' @import dplyr
#' @importFrom tibble tibble
#' @export
#' @examples
#' \dontrun{
#' get_nat_counts() %>% get_cfr()
#' }
#'

get_cfr <- function(dat) {

  tmp <- dat %>%
    dplyr::group_by(place) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup()

  tmp <- tmp %>%
    dplyr::select(
      place = place,
      C     = total_cases,
      D     = total_deaths
    )

  tmp_out <- tibble::tibble(
    place = tmp$place,
    cfr   = rep(0,nrow(tmp)),
    upper = rep(0,nrow(tmp)),
    lower = rep(0,nrow(tmp))
  )

  for (i in 1:nrow(tmp_out)) {
    C <- tmp$C[i]
    D <- tmp$D[i]

    result <- CFR(C,D)

    tmp_out$cfr[i]   <- result[1]
    tmp_out$upper[i] <- result[2]
    tmp_out$lower[i] <- result[3]

  }

  tmp_out %>%
    dplyr::mutate(
      place = case_when(
        place == "India" ~ "National estimate",
        TRUE ~ place
      )
    ) %>%
    dplyr::distinct()

}
