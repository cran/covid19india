#' Helper function
#' @param dat data set
#' @param group place variable
#' @param clmns columns to be extracted
#' @return Data set of recent observations of selected variables
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' extract_latest(dat = get_all_data())
#' }

extract_latest <- function(dat, group = place, clmns = c("total_tests", "tpr", "ppt")) {

  out <- dat[, .SD[date == max(date)], by = "place"]

  out <- out[, c("place", "date", clmns), with = FALSE]

  if ("India" %in% out[[paste0(substitute(group))]]) {

    out <- out[, place := data.table::fcase(place == "India", "National estimate", place != "India", place)]

  }

  return(out[])

}
