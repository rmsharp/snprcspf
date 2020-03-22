#' Returns non-negative results from PDL reports results
#'
#' @param pdl_df dataframe with data from one or more PDF PDL report files.
#' @import stringi
#' @export
get_pdl_nonnegative_reports <- function(pdl_df) {
  pdl_df[toupper(stri_sub(pdl_df$results, 1, 1)) != "N", ]
}
