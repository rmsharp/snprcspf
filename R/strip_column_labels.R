#' Returns content minus column labels. Fails with a call to stop()
#' if anticipated labels are not present
#'
#' @param content character vector that has lines of report with data.
#' @import stringi
#' @export
strip_column_labels <- function(content) {
  new <- character(length(content))
  i <- 0
  for (line in content) {
    if (is_column_labels(line)) {
      next
    } else {
      i <- i + 1
      new[i] <- line
    }
  }
  new[1:i]
}
