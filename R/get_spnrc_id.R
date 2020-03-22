#' Returns an SNPRC animal Id of the pdl_animal_id represents an individual
#' animal
#'
#' @param pdl_animal_id Id used by PDL to indicate the pooled sample or the
#' individual animal sample.
#' @import stringi
#' @export
get_snprc_id <- function(pdl_animal_id) {
  if (stri_detect_fixed(pdl_animal_id, pattern = "-")) {
    snprc_id <- stri_split_fixed(pdl_animal_id,
                                 pattern = "-")[[1]][2]
  } else if (stri_sub(pdl_animal_id, 1, 1) == "V") {
    snprc_id <- ""
  } else {
    snprc_id <- pdl_animal_id
  }
  if (toupper(stri_sub(snprc_id, 1, 2)) == "BN") {
    snprc_id <- stri_sub(snprc_id, 3)
  }
  snprc_id
}
