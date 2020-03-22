#' Finds any wells with low positive controls
#'
#' If either the \code{Human IgG} or the \code{Goat anti-human IgG} well
#' in the raw counts are below their respective cutoffs, the sample for that
#' well needs to be repeated.
#'
#' @param df dataframe with mean_raw_mfi_df format and data
#' @param divisors named numeric vector of divisors
#' @import stringi
#' @export
get_low_positive_controls <- function(df, divisors) {
  human_igg_divisor <-
    divisors[stri_detect_fixed(tolower(names(divisors)), "human igg") &
               !stri_detect_fixed(tolower(names(divisors)), "goat")]
  goat_anti_human_igg_divisor <-
    divisors[stri_detect_fixed(tolower(names(divisors)), "goat") &
               stri_detect_fixed(tolower(names(divisors)), " igg")]
  human_igg_vec <- (df$name == "Human IgG" &
                      !stri_detect_regex(df$sample, "iluent")) &
    df$val < human_igg_divisor
  goat_anti_human_igg_vec <- (df$name == "Goat anti-human IgG" &
                                !stri_detect_regex(df$sample, "iluent")) &
    df$val < goat_anti_human_igg_divisor
  log_vec <- human_igg_vec | goat_anti_human_igg_vec

  if (any(log_vec)) {
    low_positive_controls_df <- data.frame(
      file = df$file[log_vec],
      sample = df$sample[log_vec],
      sample_date = df$sample_date[log_vec],
      report_date = df$report_date[log_vec],
      animal_id = df$animal_id[log_vec],
      name = df$name[log_vec],
      wells = df$wells[log_vec],
      val = df$val[log_vec])
    low_positive_controls_df[!duplicated(low_positive_controls_df), ]
    low_positive_controls_df <-
      low_positive_controls_df[!is.na(low_positive_controls_df$sample), ]
  } else {
    low_positive_controls_df <- data.frame()
  }
  low_positive_controls_df
}
