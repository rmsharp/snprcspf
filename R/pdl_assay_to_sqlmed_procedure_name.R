#' Returns a character vector of SqlMed procedure_names given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_procedure_name <- function(pdl_assay) {
  procedure_name <- rep("VIRAL ANTIBODY SCREEN", length(pdl_assay))
  procedure_name[pdl_assay == "STLV PCR"] <- "STLV-1 BY PCR"
  procedure_name
}
