#' Returns ``followup'' assay results, which are groups of assay results for an
#' animal beginning with the first non-negative result and all subsequent
#' results.
#'
#' @param dsn character with the dataset name
#' @param start_date character with start date in mm-dd-yyyy format.
#' @import RODBC
#' @import stringi
#' @export
get_followup <- function(dsn, start_date = "1-1-2013") {
  sql_txt <- stri_c(
    "select CASE WHEN f.flag = '** Exited colony **' THEN f.flag + ': ' + ",
    "f.procedure_name ELSE f.flag END AS flag,
    f.id ,
    f.sample_id ,
    f.sample_date ,
    f.report_date ,
    f.procedure_name ,
    f.source ,
    f.[HERPES B VIRUS] ,
    f.[SIV AB] ,
    f.[SRV AB] ,
    f.[STLV-1 AB] ,
    f.[Herpes B Surrogate M],
    f.[HVP-2] ,
    f.[SIV WB] ,
    f.[SIV PCR] ,
    f.[SRV1 WB] ,
    f.[SRV2 WB] ,
    f.[SRV PCR] ,
    f.[STLV-1 AB PDL] ,
    f.[STLV WB] ,
    f.[STLV-1 BY PCR]
    FROM dbo.f_select_positive_surveillance() AS f
    Where f.sample_date > = '", start_date, "'
    ORDER BY f.id ASC, f.report_date asc")
  conn <- spf_odbcConnect(dsn)
  followup_df <- sqlQuery(conn, sql_txt, na.strings = "",
                          stringsAsFactors = FALSE)
  odbcClose(conn)
  followup_df$flag <-
    stri_trim_both(stri_replace_all_fixed(followup_df$flag, "*", ""))
  followup_df$`SRV PCR`[
    followup_df$`SRV PCR` == "P" &
      stri_detect_fixed(followup_df$flag, "Pooled")] <- "P (pooled)"
  followup_df$`SRV PCR`[
    followup_df$`SRV PCR` == "I" &
      stri_detect_fixed(followup_df$flag, "Pooled")] <- "I (pooled)"
  followup_df$`SRV PCR`[
    followup_df$`SRV PCR` == "N" &
      stri_detect_fixed(followup_df$flag, "Pooled")] <- "N (pooled)"
  followup_df
}
