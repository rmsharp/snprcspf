#' Returns the number of records inserted into spf_samples
#'
#' @param conn database connection object
#' @param spf_samples_df dataframe with samples
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import RODBC
#' @import stringi
#' @export
insert_spf_samples <- function(conn, spf_samples_df, run_props, run_error) {
  spf_samples_df$file_name <- basename(as.character(spf_samples_df$file_name))
  spf_samples_df$cage[is.na(spf_samples_df$cage)] <- "NULL"
  spf_samples_df$pool_id[is.na(spf_samples_df$pool_id)] <- ""
  spf_samples_df$pooled[is.na(spf_samples_df$pooled)] <- ""
  spf_samples_df$blood_expected[is.na(spf_samples_df$blood_expected)] <- ""
  spf_samples_df$blood_received[is.na(spf_samples_df$blood_received)] <- ""
  row_count <- 0
  for (i in 1:nrow(spf_samples_df)) {
    rc <- spf_samples_df[i, ]
    sql_txt <- stri_c(
      "insert into spf_samples (", vector2string(names(spf_samples_df),
                                                 SS = ", "),
      ") VALUES ('",
      rc$file_name, "', '",
      rc$snprc_id, "', ",
      rc$cage, ", '",
      rc$bleed_date, "', '",
      rc$blood_expected, "', '",
      rc$blood_received, "', '",
      rc$pool_id, "', '",
      rc$pooled, "')")
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0) {
      if (!status == "No Data")
        triggerError(run_props, run_error,
                     msg = stri_c("Insert into spf_samples failed status: ",
                                  status))
    }
    row_count <- row_count + as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  if (row_count != nrow(spf_samples_df)) {
    triggerError(run_props, run_error,
                 msg = (stri_c("Number of rows inserted into spf_samples (",
                               row_count,
                               ") is not the number of samples found (",
                               nrow(spf_samples_df), ".")))
  }
  row_count
}
