#' Returns a list of dataframes that have been converted to wide format using
#' dcast
#'
#' @param mean_raw_mfi_df dataframe to be converted to wide format
#' @param mfi_df dataframe to be converted to wide format
#' @param d_mfi_df dataframe to be converted to wide format
#' @param r_mfi_df dataframe to be converted to wide format
#' @param combined_df dataframe to be converted to wide format
#'
#' @import reshape2
#' @export
get_wide_df <- function(mean_raw_mfi_df, mfi_df, d_mfi_df, r_mfi_df,
                        combined_df) {

  agent_col <- get_col_names("agent")
  ## Using "SRV AB" to count rows is not safe. It requires for
  ## "SRV AB" to be in the assay and in every well.
  rows <- nrow(combined_df[combined_df$test_name == "SRV AB", ])
  test_rows <- nrow(combined_df[combined_df$test_name == "SIV AB", ])
  if (rows != test_rows)
    stop("SRV AB rows != SIV AB rows in get_wide_df")
  cols <- nrow(combined_df) / rows
  combined_df <- data.frame(row = rep(1:rows, each = cols), combined_df)
  w_combined_df <-
    dcast(combined_df, row + file_name + plate_id + species +
            birth_date + colony + sample +
            snprc_id + sample_date + report_date +
            repeated + wells +
            procedure_name + procedure_id ~
            agent, value.var = "assay_value")
  w_combined_df <- w_combined_df[order(w_combined_df$row), ]
  w_combined_df <-
    w_combined_df[ , c("file_name", "plate_id", "species", "birth_date",
                       "colony", "sample",
                       "snprc_id", "sample_date",
                       "report_date",
                       "repeated", "wells", "procedure_name",
                       "procedure_id",
                       agent_col[agent_col %in% names(w_combined_df)])]
  list(w_mean_raw_mfi_df = get_melted_bead_2_wide(mean_raw_mfi_df),
       w_mfi_df = get_melted_bead_2_wide(mfi_df),
       w_d_mfi_df = get_melted_bead_2_wide(d_mfi_df),
       w_r_mfi_df = get_melted_bead_2_wide(r_mfi_df),
       w_combined_df = w_combined_df)
}
