#' Returns dataframe with results from all report files in directory.
#'
#' @param base_directory path of base directory
#' base_directory = ".."
#' @param pdl_report_directory name of directory containing PDL reports
#' pdl_report_directory = "inst/extdata/"
#' @param start_year integer value indicating the first calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param end_year integer value indicating the last calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param excluded_files character vector of files within the PDL directory
#' that are to be ignored.
#' excluded_files = character(0))
#' @export
pdl_reports_to_df <- function(base_directory, pdl_report_directory,
                              start_year, end_year,
                              excluded_files = character(0)) {
  pdl_reports <- get_pdl_reports(base_directory, pdl_report_directory,
                                 start_year, end_year,
                                 excluded_files)
  pdl_df <- get_empty_pdl_df()
  for (i in seq_along(pdl_reports)) {
    pdl_df <- rbind(pdl_df, pdl_report_to_df(pdl_reports[[i]]))
  }
  add_sqlmed_codes(pdl_df, "PDL")
}
