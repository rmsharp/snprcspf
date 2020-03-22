#' Returns dataframe with parsed content of pdl_report
#'
#' @param pdl_report list containing $content and $meta that make up the
#' complete data from PDF PDL report file.
#' @import stringi
#' @export
pdl_report_to_df <- function(pdl_report) {
  file_name <- get_pdl_report_file_name(pdl_report)
  order_pk <- get_pdl_meta_data_from_content(pdl_report, "Order Pk:", 14)
  print_date_tm  <- get_pdl_meta_data_from_content(pdl_report,
                                                   "Report printed on:", 28)
  bill_po <- get_pdl_meta_data_from_content(pdl_report, "Bill Po:", 10)
  request_num <- get_pdl_meta_data_from_content(pdl_report, "Req Num:", 10)
  report_contact <- get_pdl_meta_data_from_content(pdl_report, "Rpt Contact:",
                                                   20)
  received_date <- get_pdl_meta_data_from_content(pdl_report, "Recd Dt:", 11)
  report_date  <- get_pdl_meta_data_from_content(pdl_report, "Report Dt:", 11)
  order_comment <- get_pdl_meta_data_from_content(pdl_report, # to end of line
                                                  "Order Comment:", 1000)
  content <- remove_pdl_report_headers(pdl_report$content)
  col_boundaries <- get_col_boundaries(content)
  content <- strip_column_labels(content)
  # This is needed because the report clips the last line of the page off
  # if the report goes to the next page.
  content <- insert_missing_lines(content)
  len <- length(content)
  pdl_df <- get_empty_pdl_df()
  for (i in seq_along(content)[is_odd(seq_along(content))]) {
    line_1 <- content[i]
    if (len > i) {
      line_2 <- content[i + 1]
      pdl_df <-
        rbind(pdl_df, get_one_record_df(file_name, order_pk,
                                        print_date_tm, bill_po, request_num,
                                        report_contact, received_date,
                                        report_date, order_comment, line_1,
                                        line_2, col_boundaries))
    }
  }
  pdl_df <- add_sqlmed_codes(pdl_df, "PDL")
  pdl_df$sample_type <- get_sample_type_trans(pdl_df$sample_type)
  pdl_df
}
