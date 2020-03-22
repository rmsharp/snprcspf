#' Returns the number of records inserted into pdl_results
#'
#' @param conn database connection object
#' @param pdl_df dataframe with PDL results
#' @import RODBC
#' @import stringi
#' @export
insert_pdl_results <- function(conn, pdl_df) {
  pdl_df$file_name <- basename(pdl_df$file_name)
  row_count <- 0
  for (i in 1:nrow(pdl_df)) {
    rc <- pdl_df[i, ]
    sql_txt <- stri_c(
      "insert into pdl_results (", vector2string(names(pdl_df), SS = ", "),
      ") VALUES ('", rc$file_name, "', ", rc$order_pk, ", '", rc$print_date_tm,
      "', '", rc$bill_po, "', '", rc$request_num, "', '",
      rc$report_contact, "', '",
      rc$received_date, "', '", rc$report_date, "', '", rc$order_comment, "', ",
      rc$sample, ", '",
      vector2string(rc[ , c(11:ncol(pdl_df))], SS = "', '"), "')")
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0) {
      if (!status[[1]] == "No Data") {
        stop(stri_c("insert into pdl_results failed status: ", status))
        odbcClose(conn)
      }
    }
    row_count <- row_count + as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  row_count
}
