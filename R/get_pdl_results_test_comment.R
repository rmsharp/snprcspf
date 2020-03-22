#' Returns a character vector of length 2 with the result and comment
#'
#' @param line_1 first line of data
#' @param line_2 second line of data
#' @param col_boundaries locations of start and end column for each data type
#' @import stringi
#' @export
get_pdl_results_test_comment <- function(line_1, line_2, col_boundaries) {
  r_start_end <- col_boundaries$results
  results <- stri_split_charclass(
    stri_sub(line_1, r_start_end[1]), "\\p{WHITE_SPACE}")[[1]][1]
  start_end <- col_boundaries$test_comment
  start_end[1] <- r_start_end[1] + stri_length(results)
  test_comment <-
    stri_c(stri_trim_both(stri_sub(line_1, start_end[1], start_end[2])),
           stri_trim_both(stri_sub(line_2, start_end[1], start_end[2])))
  list(results = results, test_comment = test_comment)
}
