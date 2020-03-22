#' Returns a character vector of names of usable files.
#'
#' Usable files are those from "Report plate 55 03-21-2014.xlsx" forward
#' because of large fluxuations in format.
#'
#' @param path file path to directory containing files to be read.
#' @import stringi
#' @export
get_usable_files <- function(path) {
  files <- list.files(path = path, pattern = "*.xlsx", full.names = TRUE,
                      ignore.case = TRUE)
  files <- files[!stri_detect_fixed(files, "~$R")]
  usable_file_patterns <- stri_c("Report plate ", 55:300)
  lc_files <- tolower(files)
  report_files <- as.character(sapply(tolower(usable_file_patterns),
                                      function(pattern) {
                                        files[stri_detect_fixed(lc_files, pattern = pattern)][1]}))
  spf_files <- files[stri_detect_fixed(lc_files,
                                       pattern = "spf colony testing")]
  files <- unique(c(report_files, spf_files))
  files[!is.na(files)]
}
