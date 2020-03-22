#' Returns a list of lists of length two containing $content and $meta lists
#' derived from each PDL report.
#'
#' The returned list contains one list of length 2 for each PDL report.
#' The 2 elements within each list are the content list, which has one element
#' for each line within the report and the meta list, which has 7 elements.
#' See get_pdl_meta_data_from_content().
#'
#' @param base_directory character vector with path for directory in which the
#' pdl_report_directory resides.
#' @param pdl_report_directory character vector with name of directory in which
#' the PDL reports reside.
#' @param start_year integer value indicating the first calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param end_year integer value indicating the last calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param excluded_files character vector of files to ignore.
#' @import tm
#' @export
get_pdl_reports <- function(base_directory = "../inst/extdata/",
                            pdl_report_directory = "PDL/",
                            start_year, end_year,
                            excluded_files = character(0)) {
  files <- c()
  for (year in stri_c(seq(start_year, end_year))) {
    path <- stri_c(base_directory, pdl_report_directory, year)
    files <- c(files, list.files(path, recursive = TRUE, pattern = ".pdf",
                                 full.names = TRUE))
  }
  #total_number_of_files <- length(files)
  for (excluded_file in excluded_files) {
    files <- files[!(stri_detect_fixed(files, excluded_file))]
  }

  pdl_reports <- list(length(files))
  for (i in seq_along(files)) {
    uri <- sprintf(stri_c("file://", "%s"),
                   files[i])
    if (all(file.exists(Sys.which(c("pdftotext",
                                    "pdfinfo"))))) {
      pdl_df <- readPDF(control = list(text = "-layout"))(
        elem = list(uri = uri), language = "en", id = "id1")
    }
    pdl_reports[i] <- list(pdl_df)
  }
  pdl_reports
}
