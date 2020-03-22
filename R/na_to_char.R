#' Replaces an NA value with a character string
#'
#' @param x character vector
#' @param char character string used to replace NAs
#' @export
na_to_char <- function(x, char) {
  x[is.na(x)] <- char
  as.character(x)
}
