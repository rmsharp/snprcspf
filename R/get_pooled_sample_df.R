#' Returns dataframe with animal Ids, cage location, bleed date, whether not a
#' blood sample was expected, whether or not a sample was received, and the
#' Pool Id number.
#'
#' @param path charcter vector of length 1 having the path section of the
#' fully qualified file name.
#' @import stringi
#' @export
get_pooled_sample_df <- function(path) {
  sample_df <- get_sample_df(path)
  sample_df[sample_df$pooled == "Y", ]
}
