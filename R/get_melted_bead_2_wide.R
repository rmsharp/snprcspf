#' Returns bead based wide dataframe from bead based melted dataframe.
#'
#' @param .df bead based melted dataframe
#' @import reshape2
#' @import stringi
#' @export
get_melted_bead_2_wide <- function(.df) {
  bead_col <- get_col_names("bead")
  rows <- nrow(.df[.df$name == "SRV-2", ])
  cols <- nrow(.df) / rows
  .df <- data.frame(row = rep(1:rows, cols), .df)
  if (any(stri_detect_fixed(names(.df), "result"))) {
    value_var <- "result"
  } else {
    value_var <- "val"
  }
  w_df <- dcast(.df, row + file + sample + animal_id + sample_date +
                  report_date + wells ~
                  name, value.var = value_var)
  w_df <- w_df[order(w_df$row), ]
  w_df <-
    w_df[ , c("file", "sample",
              "animal_id", "sample_date", "report_date", "wells",
              bead_col[bead_col %in% names(w_df)])]
  w_df
}
