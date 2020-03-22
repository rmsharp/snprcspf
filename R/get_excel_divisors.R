#' Returns a named numeric vector with the divisors originally provided by the
#' vendor and copied into the Excel sheet "../inst/extdata/cutoff.xlsx"
#'
#' @param results character vector with results section of the Luminex file
#' being analyzed.
#' @import readxl
#' @import stringi
#' @export
get_excel_divisors <- function(results) {
  for (i in 1:nrow(results)) {
    if (any(tolower(results[i, ]) %in% "cutoff"))
      break
  }
  if (i >= nrow(results)) {
    stop(stri_c("Cutoff values were not found in get_excel_divisors().
                'cutoff' was not found in the ", nrow(results),
                " rows examined."))
  }
  start <- i
  row <- results[start, ]
  row[is.na(row)] <- ""
  name_col <- min((1:length(row))[stri_detect_fixed(tolower(row), "name")])
  cutoff_col <- (1:length(row))[stri_detect_fixed(tolower(row), "cutoff")]
  start <- start + 1
  results <- data.frame(results[start:nrow(results), ])
  rows <- 1:(which(is.na(results[ , name_col]))[1] - 1)
  #rows <- seq_along(results[ , name_col][!is.na(results[ , name_col])])
  divisors <- results[rows, cutoff_col]
  divisors <- as.numeric(divisors)
  divisor_names <- results[rows, name_col]
  if (!any(stri_detect_fixed(divisor_names, pattern = "rBV glycoB"))) {
    divisor_names[stri_detect_regex(tolower(divisor_names),
                                    pattern = "glyco")] <- "BV glyco B"
  }
  names(divisors) <- divisor_names
  divisors
}
