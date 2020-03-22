#' Returns "Y" if a sample is pooled and "N" if it is an individual sample.
#'
#' @param pdl_animal_id Id used by PDL to indicate the pooled sample or the
#' individual animal sample.
#' @import stringi
#' @export
pooled_Y_or_N <- function(pdl_animal_id) {
  if (toupper(stri_sub(pdl_animal_id, 1, 1)) == "V") {
    pooled <- "Y"
  } else {
    pooled <- "N"
  }
  pooled
}
