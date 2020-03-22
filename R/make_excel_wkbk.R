#' Form an Excel workbook and worksheet similar in format to those being
#' made by Luis Giavedoni's laboratory.
#'
#' The Excel file ``Report plate 62 08-15-2014.xlsx'' was used as a guide.
#' @param file character vector of length one with name of file to create
#' @param w_raw_mfi_df dataframe with raw results from Luminex machine
#' @param w_mfi_df dataframe with background substracted from raw results
#' @param w_d_mfi_df dataframe with normalized values where divisors were
#' used on background subtracted results
#' @param w_r_mfi_df dataframe where each antigen result is called as N, I, or
#' P based on is values relative to a fraction of the low control value.
#' @param w_combined_df dataframe with the antigen calls combined into agent
#' (virus) calls. (See get_combined() for algorithm).
#' @param low_positive_controls_df dataframe with \code{sample}, \code{name},
#' \code{wells}, and \code{val} columns that identify the well with low
#' positive control values (<5000 for Human or Goat IgG).
#'
#' @import rmsutilityr
#' @export
make_excel_wkbk <- function(file, w_raw_mfi_df, w_mfi_df, w_d_mfi_df,
                            w_r_mfi_df, w_combined_df,
                            low_positive_controls_df) {
  w_raw_mfi_df$file <- basename(w_raw_mfi_df$file)
  w_mfi_df$file <- basename(w_mfi_df$file)
  w_d_mfi_df$file <- basename(w_d_mfi_df$file)
  w_r_mfi_df$file <- basename(w_r_mfi_df$file)
  w_combined_df$file_name <- basename(w_combined_df$file_name)
  if (nrow(low_positive_controls_df) > 0) {
    df_list <- list(
      w_raw_mfi_df,
      w_mfi_df,
      w_d_mfi_df,
      w_r_mfi_df,
      w_combined_df,
      low_positive_controls_df)
    sheetnames <- c(
      "raw_mfi",
      "minus_background",
      "normalized",
      "antigen_called",
      "final_result",
      "low_positive_controls")
  } else {
    df_list <- list(
      w_raw_mfi_df,
      w_mfi_df,
      w_d_mfi_df,
      w_r_mfi_df,
      w_combined_df)
    sheetnames <- c(
      "raw_mfi",
      "minus_background",
      "normalized",
      "antigen_called",
      "final_result")
  }

  create_wkbk(file, df_list, sheetnames)
  ## code disabled to allow temporary use under Java 12 environment, which has a
  ## bug preventing use of XLConnect
  # sheets_index <- c(3, 4, 5)
  # for (i in seq_along(sheets_index)) {
  #   df_index <- c(4, 4, 5) # normalized sheet gets same format as
  #                                 # antigen sheet
  #   format_luminex_results(file, df_list[[df_index[i]]],
  #                          sheetnames[sheets_index[i]],
  #                          low_positive_controls_df)
  # }
  file
}
