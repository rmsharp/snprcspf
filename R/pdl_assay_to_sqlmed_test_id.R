#' Returns a character vector of SqlMed test_id given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_test_id <- function(pdl_assay) {
  as.integer(c(
    "SIV AB" = 788,
    "SIV MIA" = 788,
    "SIV PCR" = 852,
    "SIV WB" = 948,
    "HERPES B MIA" = 785,
    "HVP2 MIA" = 917,
    "MEASLES MIA" = 786,
    "SRV MIA" = 787,
    "SRV AB" = 787,
    "SRV PCR" = 875,
    "SRV1 WB" = 943,
    "SRV2 WB" = 944, # this is made up
    "STLV MIA" = 793,
    "STLV AB" = 793,
    "STLV PCR" = 898,
    "STLV WB" = 947)[pdl_assay])
}
