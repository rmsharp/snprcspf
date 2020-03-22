#' Returns mfi_df dataframe values with wBAC column subtracted from each
#' value and the wBAC and the Human IgG, wBAC and Goat anti-human IgG rows
#' removed from raw_mfi_df
#'
#' @param raw_mfi_df dataframe with raw data
#' @export
subtract_background <- function(raw_mfi_df) {
  df <- data.frame(row_order = 1:nrow(raw_mfi_df), raw_mfi_df)
  samples <- unique(df$sample)
  for (sample in samples) {
    df$val[df$name != "wBAC" & df$sample == sample] <-
      df$val[df$name != "wBAC" & df$sample == sample] -
      df$val[df$name == "wBAC" & df$sample == sample]
  }
  df <- df[order(df$row_order), -1]
  mfi_df <- df[!df$name %in% c("wBAC", "Human IgG",
                               "Goat anti-human IgG"), ]
  mfi_df
}
