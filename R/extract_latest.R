#' Helper function
#' @param dat data set
#' @param group place variable
#' @param cols columns to be extracted
#' @return Data set of recent observations of selected variables
#' @import dplyr
#' @importFrom tidyselect all_of
#' @export
#' @examples
#' \dontrun{
#' get_all_data() %>% extract_latest()
#' }

extract_latest <- function(dat, group = place, cols = c("total_tests", "tpr", "dbl", "ppt")) {

  out <- dat %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::distinct(date, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select({{ group }}, date, tidyselect::all_of(cols))

  if ("India" %in% out[[paste0(substitute(group))]]) {

    out[[paste0(substitute(group))]] <- dplyr::recode(out[[paste0(substitute(group))]],
                                               "India" = "National estimate")

  }

  return(out)

}
