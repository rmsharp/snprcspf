#' Returns content for PDL reports without introductory material and end of
#' page material
#'
#' @param content copy of pdl_report$content taken from PDF PDL report
#' @export
remove_pdl_report_headers <- function(content) {
  new <- character(length(content))
  j <- 1
  for (i in seq_along(content)) {
    line <- content[i]
    if (!stri_detect_fixed(line, "Sample #")) {
      next
    } else {
      break
    }
  }
  for (k in i:length(content)) {
    line <- content[k]
    if (stri_detect_fixed(line, "Pathogen") |
        stri_detect_fixed(line, "Report") |
        stri_detect_fixed(line, "\f") |
        stri_detect_fixed(line, "Page") |
        stri_detect_fixed(line, "Order") |
        stri_detect_fixed(line, "Bill")) {
      next
    } else {
      new[j] <- line
      j <- j + 1
    }
  }
  new[nchar(stri_trim_both(new)) > 0]
}
