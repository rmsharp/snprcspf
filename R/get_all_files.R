#' Returns the list of pathology report files with an extension of ".pdf"
#' found in folders indicated by the years between start_year and end_year
#' inclusive.
#'
#' @param start_year integer value indicating the first calendar year as used
#' in the name of the folders containing reports from the years indicated.
#' @param end_year integer value indicating the last calendar year as used in
#' the name of the folders containing reports from the years indicated.
get_all_files <- function(start_year, end_year) {
  files <- c()
  for (year in stri_c(seq(start_year, end_year))) {
    path <-  stri_c("/Volumes/Pathology/Path\ Reports/", year, "\ Reports/")
    #path <- stri_c("/Volumes/Path Reports/", year, "\ Reports")
    #path <- stri_c("../data/", year, " Reports/")
    files <- c(files, list.files(path, pattern = ".pdf", full.names = TRUE))
  }
  files
}
