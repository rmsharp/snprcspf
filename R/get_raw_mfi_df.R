#' Returns dataframe with raw mfi values from the raw section of the Excel
#' file.
#'
#' @param conn database connection object
#' @param df character vector with line from the Excel file worksheet
#' having only the result table records
#' @param file character vector of length one with filename
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @return \code{raw_mfi_df} dataframe
#' @import reshape2
#' @import stringi
#' @export
get_raw_mfi_df <- function(conn, df, file, run_props, run_error) {
  wide_raw_df <- data.frame(
    file = rep(file, nrow(df)),
    sample = df$sample,
    animal_id = get_id_from_sample(conn, df$sample),
    sample_date = get_date_from_sample(conn, df$sample),
    report_date = get_report_date_from_filename(file, run_props, run_error),
    df[ , -1],
    stringsAsFactors = FALSE, check.names = FALSE
  )
  raw_df <- melt(wide_raw_df,
                 id.vars = c("file", "sample", "animal_id",  "sample_date",
                             "report_date", "wells"),
                 measure.vars = names(wide_raw_df)[
                   7:length(names(wide_raw_df))],
                 variable.name = "name",
                 value.name = "val", factorsAsStrings = TRUE)
  raw_df$wells <- sapply(raw_df$wells, function(well) {
    if (stri_sub(well, stri_length(well), stri_length(well)) == ",") {
      well <- stri_sub(well, 1, stri_length(well) - 1)
    } else {
      well
    }})

  raw_df$val <- suppressWarnings(as.numeric(raw_df$val))
  raw_df
}
