#' Returns dataframe with means of replicated samples
#'
#' @param raw_mfi_df dataframe with raw mfi reading
#' @import stringi
#' @export
get_mean_raw_mfi <- function(raw_mfi_df) {
  df <- data.frame(row_order = 1:nrow(raw_mfi_df), raw_mfi_df)
  mean_raw_mfi <- data.frame()
  col_names <- c("row_order", "file", "sample", "animal_id", "sample_date",
                 "report_date", "wells", "name", "val")
  for (sample in unique(df$sample)) {
    for (name in unique(df$name)) {
      tmp_df <- df[df$name == name & df$sample == sample, ]
      val <- mean(tmp_df$val)
      wells <- stri_c(tmp_df$well_row, tmp_df$well_col, collapse = ",")
      tmp_df <- tmp_df[1, ]
      tmp_df$wells <- wells
      tmp_df$val <- val
      mean_raw_mfi <- rbind(mean_raw_mfi, tmp_df[ , col_names])
    }
  }
  mean_raw_mfi <- mean_raw_mfi[order(mean_raw_mfi$row_order), -1]
  mean_raw_mfi
}
