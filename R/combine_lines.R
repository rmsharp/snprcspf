#' Returns character vector of length 1 made up of text found in two character
#' vector elements as subsetted according to start and end locations.
#'
#' @param line_1 first line of text to subset
#' @param line_2 second line of text to subset
#' @param start_end column boundaries to include
#' @importFrom stringi stri_c stri_trim_both stri_sub
#' @export
combine_lines <- function(line_1, line_2, start_end) {
  stri_c(stri_trim_both(stri_sub(line_1, start_end[1], start_end[2])),
         stri_trim_both(stri_sub(line_2, start_end[1], start_end[2])))
}
