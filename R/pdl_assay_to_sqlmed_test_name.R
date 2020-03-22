#' Returns a character vector of SqlMed test_name given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_test_name <- function(pdl_assay) {
  as.character(c(
    "SIV AB" = "SIV AB",
    "SIV MIA" = "SIV AB",
    "SIV PCR" = "SIV PCR",
    "SIV WB" = "SIV WB",
    "HERPES B MIA" = "HERPES B VIRUS",
    "HVP2 MIA" = "HVP-2",
    "MEASLES MIA" = "MEASLES",
    "SRV MIA" = "SRV AB",
    "SRV PCR" = "SRV PCR",
    "SRV1 WB" = "SRV1 WB",
    "SRV2 WB" = "SRV2 WB", # this is made up
    "STLV MIA" = "STLV-1 AB",
    "STLV AB" = "STLV-1 AB",
    "STLV PCR" = "STLV-1 BY PCR",
    "STLV WB" = "STLV WB")[pdl_assay])
}
