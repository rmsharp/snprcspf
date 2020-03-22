#' Returns a character vector of 'Y' if the sample indicates it is a
#' repeat else 'N'
#'
#' @param sample character vector with one or more sample Ids
#' @import stringi
#' @export
get_repeated_from_sample <- function(sample) {
  ifelse(toupper(stri_sub(sample, -2)) == "-R", "Y", "N")
}
