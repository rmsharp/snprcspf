#' Returns TRUE if a line is an expected set of column labels
#'
#' @param line character vector of length 1.
#' @import stringi
is_column_labels <- function(line) {
  expected <- c("Sample", "#", "An", "Id", "Sp", "Type", "Date", "Comment",
                "Test", "Type", "Results", "Test", "Comment")
  observed <- stri_trim_both(stri_split_boundaries(line, simplify = TRUE)[1, ])
  all(is.element(expected, observed))
}
