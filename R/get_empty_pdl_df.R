#' Returns empty pdl_df
#'
#' @export
get_empty_pdl_df <- function() {
  data.frame(file_name = character(0),
             order_pk = character(0),
             print_date_tm = character(0),
             bill_po = integer(0),
             request_num = character(0),
             report_contact = character(0),
             received_date  = character(0),
             report_date = character(0),
             order_comment = character(0),
             sample = integer(0),
             pdl_animal_id = character(0),
             snprc_id = character(0),
             pooled = character(0),
             pdl_species = character(0),
             sample_type = character(0),
             sample_date = character(0),
             comment = character(0),
             test_type = character(0),
             procedure_id = integer(0),
             procedure_name = character(0),
             test_id = integer(0),
             test_name = character(0),
             results = character(0),
             test_comment = character(0),
             stringsAsFactors = FALSE)
}
