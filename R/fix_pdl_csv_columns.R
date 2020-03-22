#' Returns pdl_df with columns renamed, added and ordered to prepare the
#' dataframe for inserting into the pdl_result table.
#'
#' @param pdl_df dataframe as it comes from read.csv()
#' @import rmsutilityr
#' @import stringi
#' @export
fix_pdl_csv_columns <- function(pdl_df) {
  names(pdl_df) <- c("file_name", "print_date_tm", "order_pk", "client",
                     "bill_po", "request_num", "received_date", "report_date",
                     "order_comment", "var_9", "l_name", "f_name",
                     "sample", "pdl_animal_id", "pdl_species", "sample_type",
                     "sample_date", "comment", "var_3", "var_4", "panel",
                     "var_5", "test_type", "results", "test_comment", "var_6")
  pdl_df$report_contact <- stri_c(pdl_df$l_name, rep(", ", nrow(pdl_df)),
                                  pdl_df$f_name)
  pdl_df$test_comment <- stri_c(pdl_df$test_comment, rep(", ",
                                                         nrow(pdl_df)),
                                pdl_df$var_6)
  pdl_df$var_6 <- NULL
  pdl_df$snprc_id <- suppressWarnings(
    sapply(pdl_df$pdl_animal_id, function(id) {get_snprc_id(id)}))
  pdl_df$pooled <- sapply(pdl_df$pdl_animal_id, function(id) {
    pooled_Y_or_N(id)})
  pdl_df <- add_sqlmed_codes(pdl_df, "PDL")
  pdl_df <- pdl_df[ , c("file_name", "order_pk", "print_date_tm",
                        "bill_po", "request_num", "report_contact",
                        "received_date", "report_date", "order_comment",
                        "sample", "pdl_animal_id", "snprc_id",
                        "pooled", "pdl_species",  "sample_type",
                        "sample_date", "comment", "test_type",
                        "results", "test_comment", "procedure_id",
                        "procedure_name", "test_id", "test_name")]#,
  #    "var_9", "l_name", "f_name", "client",
  #    "var_3", "var_4",
  #    "var_5")]
  pdl_df
}
