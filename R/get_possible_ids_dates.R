#' Returns a list of length 2 with possible$ids and possible$dates
#'
#' @param samples character vector of sample descriptors
#' @import stringi
#' @export
get_possible_ids_dates <- function(samples) {
  samples <- remove_bn_and_dash_R(samples)
  ids <- sapply(stri_split_regex(samples, "[ -]"), function(x) {
    x[[1]][1]})
  ids[!stri_detect_regex(ids, "[0-9]")] <- NA
  dates <- sapply(stri_split_regex(samples, "[ -]"), function(x) {
    len <- length(x)
    if (len > 1) {
      x[length(x)]
    } else {
      NA
    }
  })
  dates[!stri_detect_regex(dates, "[0-9]")] <- NA
  dates <- get_yyyymmdd_from_possible_dates(dates)
  ids <- get_yyyymmdd_from_possible_dates(ids)
  list(ids = ids, dates = dates)
}
