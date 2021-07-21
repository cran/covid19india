#' Helper function for pulling latest R estimates
#' @param dat data set containing R estimates
#' @return Pulls 7-day trailing average R estimates and 95% confidence intervals
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' get_all_data() %>% get_r_est()
#' }

get_r_est <- function(dat) {

  dat %>%
    dplyr::group_by(place) %>%
    dplyr::slice((n()-6):n()) %>%
    dplyr::summarize(
      r       = mean(r_est, na.rm = TRUE),
      lower   = mean(r_lower, na.rm = TRUE),
      upper   = mean(r_upper, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup()

}
