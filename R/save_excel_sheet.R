#' Returns the file name of the Excel file created which contains the
#' dataframes provided as an arguments to the function.
#'
#' @param sorted_df a datafrome containing the sorted followup assay results.
#' @param still_pos_df a datafrome containing the sorted followup assay results
#' by animal where the assay results are still non-negative.
#' @return name of Excel file saved containing the sorted followup dataframe
#' (\code{sorted_df}).
#' @import lubridate
#' @import rmsutilityr
#' @export
save_excel_sheet <- function(sorted_df, still_pos_df) {
  prefix <- stri_c(
    stri_replace_all_fixed(stri_replace_all_fixed(now(), " ", "_"), ":", "-"),
    "_")
  col_names <- remove_strings(names(sorted_df), c("sample_id", "report_date",
                                                  "procedure"))
  excel_file <- stri_c("../reports/", prefix, "followup", ".xlsx")
  create_wkbk(excel_file, list(sorted_df[ , col_names], still_pos_df),
              c("followup", "still_positive"))
  excel_file
}
