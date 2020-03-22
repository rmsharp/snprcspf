#' Returns the number of saved records
#'
#' @param conn database connection object
#' @param r_df dataframe containing the melted version of the individual bead
#' @param table optional character variable with name of table to insert into.
#' assay
#' results.
#' @import RODBC
#' @import stringi
#' @export
insert_combined_assay_results <- function(conn, r_df,
                                          table = "luminex_screen_results") {
  r_df <- r_df[!is.na(r_df$snprc_id), ]
  r_df$file_name <- basename(r_df$file_name)
  row_count <- 0
  for (i in seq_along(r_df$snprc_id)) {
    r_df[i, ][is.na(r_df[i, ])] <- ""
    sql_txt <- stri_c(
      "INSERT INTO ", table, " (
      file_name, plate_id, sample, species, snprc_id, sample_date,
      report_date, repeated, agent,
      procedure_id, procedure_name, test_id, test_name, assay_value)
      values ('", vector2string(as.character(
        r_df[i, c("file_name", "plate_id", "sample", "species", "snprc_id",
                  "sample_date", "report_date",
                  "repeated", "agent", "procedure_id", "procedure_name",
                  "test_id", "test_name", "assay_value")]), "', '"), "') ")
    #cat(stri_c(sql_txt, "\n"))
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0) {
      if (!status == "No Data")
        stop(stri_c("insert into ", table, " failed status: ",
                    status))
    }
    row_count <- row_count + as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  row_count
}
