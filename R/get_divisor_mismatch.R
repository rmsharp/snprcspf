#' Returns a charcter vector with the file names of files that have region
#' names that do not
#' match those found in ``cutoff.xlsx''
#'
#' @param file_names character vector of file names with XML data
#' @export
get_divisor_mismatch <- function(file_names) {
  file_list <- character(length(file_names))
  count <- 0
  for (file in file_names) {
    if (!divisors_match(file)) {
      count <- count + 1
      file_list[count] <- file
    }
  }
  file_list <- file_list[!is.na(file_list)]
}
