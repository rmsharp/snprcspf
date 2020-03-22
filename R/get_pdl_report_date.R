#' Returns report date from a pdl_report object
#'
#' @param pdl_report list containing $content and $meta that make up the
#' complete data from PDF PDL report file.
#' @export
get_pdl_report_date <- function(pdl_report) {
  pdl_report$meta$datetimestamp
}
