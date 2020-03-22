#' Returns specified value from a pdl_report$content object
#'
#' @param pdl_report list containing $content and $meta that make up the
#' complete data from PDF PDL report file.
#' @param meta_data_label character vector of string to search for that
#' is immediately followed by the meta data being sought
#' @param len maximum length of the meta data being retrieved.
#' @import stringi
#' @export
get_pdl_meta_data_from_content <- function(pdl_report, meta_data_label, len) {
  content <- pdl_report$content
  meta_data <- ""
  for (line in content) {
    if (stri_detect_fixed(line, meta_data_label)) {
      from <- stri_locate_first_fixed(line, pattern = meta_data_label)[2] + 1
      meta_data <- stri_trim_both(stri_sub(line, from = from, length = len))
      break
    }
  }
  meta_data
}
