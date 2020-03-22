#' Returns  positive antigens based on low control value and a multiplier
#'
#' From e-mail 2015-09-22 13:32 by Luis Giavedoni
#'
#' Mark,
#'
#' When the CNT LOW is below 2.5, we use the CNT LOW score to assign
#' positive reactivity. Any score equal or higher than the CNT LOW will be
#' scored as positive.
#'
#' When the CNT LOW is above 2.5, we use 2.5 as the cut off. Any score equal
#' or higher than 2.5 will be scored as positive.
#'
#' I know that I said that the low score was 3.0, but after review of
#' current Charles River documents, the recommendation is for the score to be
#' 2.5.
#'
#' Regards,
#'
#' Luis
#'
#' Thus, the function used is
#' df$result <- ifelse(df$val >= min(2.5, df$cutoff), "P", "N")
#'
#' \code{SPF Colony Testing Plate 96   06-09-16.xlsx} has an extra low control
#' \code{ICL CNT LOW (dup)}.
#' Controls with \code{stri_detect_fixed(tolower(name), "(dup)") == TRUE}
#'  are ignored.
#'
#' @param d_mfi_df dataframe wih standardized antigen reactivity measures
#' @param mult multiplier used to adjust cutoff level.
#' @import stringi
#' @export
get_positive_antigens <- function(d_mfi_df, mult = 1.0) {
  df <- data.frame(row_order = 1:nrow(d_mfi_df), d_mfi_df)
  df$cutoff <- df$val
  df$result <- rep("NONE", nrow(df))
  for (name in unique(df$name)) {
    for (sample in unique(df$sample)) {
      if (
        length(df$sample[
          stri_detect_fixed(tolower(df$sample), pattern = "low") &
          !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
          !stri_detect_regex(df$sample, pattern = "[0-9]") &
          df$name == name]) == 1) { # A
        df[df$sample == sample & df$name == name, "cutoff"] <-
          df[stri_detect_fixed(tolower(df$sample), pattern = "low") &
               !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
               !stri_detect_regex(df$sample, pattern = "[0-9]") &
               df$name == name, "val"] * mult
      } else if (
        length(df$sample[
          stri_detect_fixed(tolower(df$sample), pattern = "low") &
          !stri_detect_regex(tolower(df$sample), pattern = "lot") &
          !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
          !stri_detect_regex(df$sample, pattern = "[0-9]") &
          df$name == name]) == 1) { # A
        df[df$sample == sample & df$name == name, "cutoff"] <-
          df[stri_detect_fixed(tolower(df$sample), pattern = "low") &
               !stri_detect_regex(tolower(df$sample), pattern = "lot") &
               !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
               !stri_detect_regex(df$sample, pattern = "[0-9]") &
               df$name == name, "val"] * mult
      } else {
        stop("Low positive control value is missing or there is more than one.")
      }
    }
  }
  df$result <- ifelse(df$val >= pmin(2.5, df$cutoff), "P", "N")
  df <- df[order(df$row_order), -1]
  df
}
