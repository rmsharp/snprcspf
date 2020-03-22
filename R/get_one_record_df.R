#' Returns a one record dataframe with the data from one assay record.
#'
#' @param file_name name of file according to $meta$id
#' @param order_pk order primary key
#' @param print_date_tm print date of report
#' @param bill_po bill number
#' @param request_num request number
#' @param report_contact report contact
#' @param received_date date samples were received
#' @param report_date date the report was generated according to
#' $meta$datetimestamp
#' @param order_comment comment provided by SNPRC about the order
#' @param line_1 first line of data
#' @param line_2 second line of data
#' @param col_boundaries locations of start and end column for each data type
#' @export
get_one_record_df <- function(file_name, order_pk,
                              print_date_tm, bill_po, request_num,
                              report_contact, received_date,
                              report_date, order_comment, line_1,
                              line_2, col_boundaries) {
  #cat(stri_c(line_1, "\n"))
  results_test_comment <-
    get_pdl_results_test_comment(line_1, line_2, col_boundaries)
  data.frame(file_name = file_name,
             order_pk = order_pk,
             print_date_tm = print_date_tm,
             bill_po = bill_po,
             request_num = request_num,
             report_contact = report_contact,
             received_date = received_date,
             report_date = report_date,
             order_comment = order_comment,
             sample = combine_lines(line_1, line_2,
                                    col_boundaries$sample),
             pdl_animal_id = combine_lines(line_1, line_2,
                                           col_boundaries$pdl_animal_id),
             snprc_id = get_snprc_id(combine_lines(
               line_1, line_2, col_boundaries$pdl_animal_id)),
             pooled = pooled_Y_or_N(combine_lines(
               line_1, line_2, col_boundaries$pdl_animal_id)),
             pdl_species = combine_lines(line_1, line_2,
                                         col_boundaries$pdl_species),
             sample_type = combine_lines(line_1, line_2,
                                         col_boundaries$sample_type),
             sample_date = combine_lines(line_1, line_2,
                                         col_boundaries$sample_date),
             comment = combine_lines(line_1, line_2,
                                     col_boundaries$comment),
             test_type = combine_lines(line_1, line_2,
                                       col_boundaries$test_type),
             results = results_test_comment$results,
             test_comment = results_test_comment$test_comment,
             stringsAsFactors = FALSE)

}
