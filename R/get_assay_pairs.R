#' Returns a dataframe, with sets of assays with an antibody assay (ab_assay)
#' and a PCR based assay (pcr_assay) for each agent.
#'
get_assay_pairs <- function() {
  data.frame(agent = c("SIV", "SRV", "STLV-1"),
             ab_assay = c("SIV AB", "SRV AB", "STLV-1 AB"),
             pcr_assay = c("SIV PCR", "SRV PCR",
                           "STLV-1 BY PCR"),
             stringsAsFactors = FALSE) # no HERPES test
}
