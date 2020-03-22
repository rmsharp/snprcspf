#' Returns assay results for individual animals based on assay results of pooled
#' samples.
#'
#' @param followup_df dataframe with animal Ids to use in selected pool sample
#' results.
#' @import RODBC
#' @import stringi
#' @export
get_followup_pool_samples <- function(followup_df) {
  pooled_df <- get_pooled_sample_df("../data/pool_definitions/")
  pooled_df <- pooled_df[pooled_df$snprc_id %in% unique(followup_df$id) &
                           pooled_df$blood_received == "yes", ]
  pooled_df <- pooled_bleed_date_to_sample_date(pooled_df, sep = "-")
  conn <- spf_odbcConnect("frogstar-vortex-animal-sa")
  sql_txt <- stri_c(
    "select * from pdl_results where substring(pdl_animal_id, 1, 1) = 'V'"
  )
  pdl_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  odbcClose(conn)
  pdl_df <- pdl_df[ , c("pdl_animal_id", "sample_date", "report_date",
                        "procedure_name", "test_id",
                        "test_name", "results")]
  pdl_df$results[toupper(stri_sub(pdl_df$results, 1, 1)) == "P"] <- "P"
  pdl_df$results[toupper(stri_sub(pdl_df$results, 1, 1)) == "I"] <- "I"
  pdl_df$results[toupper(stri_sub(pdl_df$results, 1, 1)) == "N"] <- "N"
  pooled_df <- merge_pooled_and_pdl(pooled_df, pdl_df)

  pooled_df <- data.frame(flag = rep("Pooled SRV PCR", nrow(pooled_df)),
                          id = pooled_df$snprc_id,
                          sample_id = stri_c(pooled_df$snprc_id,
                                             rep(" - ", nrow(pooled_df)),
                                             pooled_df$sample_date),
                          sample_date = pooled_df$sample_date,
                          report_date = pooled_df$report_date,
                          procedure_name = pooled_df$procedure_name,
                          source = rep("PDL", nrow(pooled_df)),
                          `HERPES B VIRUS` = rep(NA, nrow(pooled_df)),
                          `SIV AB` = rep(NA, nrow(pooled_df)),
                          `SRV AB` = rep(NA, nrow(pooled_df)),
                          `STLV-1 AB` = rep(NA, nrow(pooled_df)),
                          `Herpes B Surrogate M` = rep(NA, nrow(pooled_df)),
                          `HVP-2` = rep(NA, nrow(pooled_df)),
                          `SIV WB` = rep(NA, nrow(pooled_df)),
                          `SIV PCR` = rep(NA, nrow(pooled_df)),
                          `SRV1 WB` = rep(NA, nrow(pooled_df)),
                          `SRV2 WB` = rep(NA, nrow(pooled_df)),
                          `SRV PCR` = as.character(pooled_df$results),
                          `STLV-1 AB PDL` = rep(NA, nrow(pooled_df)),
                          `STLV WB` = rep(NA, nrow(pooled_df)),
                          `STLV-1 BY PCR` = rep(NA, nrow(pooled_df)),
                          stringsAsFactors = FALSE, check.names = FALSE)
  pooled_df
}
