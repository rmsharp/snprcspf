#' Returns character vector representing the contents of the pdl_report$content
#' with the missing second lines of data replaced.
#'
#' @param content character vector representing the contents of the
#'  pdl_report$content with the possible missing second lines of data
#' @import stringi
#' @export
insert_missing_lines <- function(content) {
  new <- character(length(content))
  j <- 0
  last_line <- "second"
  second_line <- "          "
  for (line in content) {
    len <- nchar(stri_trim_both(stri_sub(line, from = 1, to = 10)))
    if (len == 0) {
      j <- j + 1
      new[j] <- line
      last_line <- "second"
    } else if (last_line == "first" & len > 0) {
      #last_line <- "second"
      j <- j + 1
      new[j] <- second_line
      j <- j + 1
      new[j] <- line
    } else if (last_line == "second" & len > 0) {
      last_line <- "first"
      j <- j + 1
      new[j] <- line
    }
  }
  if (last_line == "first") {
    j <- j + 1
    new[j] <- second_line
  }
  new[1:j]
}
