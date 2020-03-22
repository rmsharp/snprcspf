#' Returns a character vector of SqlMed procedure_id given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_procedure_id <- function(pdl_assay) {
  procedure_id <- ifelse(pdl_assay == "STLV PCR", 10620, 10616)
  procedure_id
}
