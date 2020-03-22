#' Returns the corrected sample type given slightly malformed or incomplete
#' sample types.
#'
#' @param sample_type character vector of sample types to be translated.
#' @import stringi
#' @export
get_sample_type_trans <- function(sample_type) {
  sample_type[stri_detect_fixed(sample_type, "WHOLE")] <- "WHOLE BLOOD"
  sample_type
}
