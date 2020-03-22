#' Returns a list of samples that were expected but not
#' received.
#'

#' @param sample_df dataframe from sample file
#' @param file_name fully qualified file name of Excel file
#' @import rmsutilityr
#' @import stringi
#' @export
report_unexpected_samples <- function(sample_df, file_name) {
  if (!all(is.na(sample_df$snprc_id[sample_df$blood_received == "yes"]))) {
    unexpected_samples <-
      sample_df[sample_df$blood_received == "yes" &
                  sample_df$blood_expected == "no", ]
    if (nrow(unexpected_samples) > 0) {
      unexpected_samples$cage <- round(unexpected_samples$cage, 2)
      create_wkbk(file = "../reports/unexpected_samples_log.xlsx",
                  list(unexpected_samples), "unexpected samples")
      warning(stri_c("There are one or more blood samples that were not ",
                     "expected were received in a sample. They are listed in ",
                     "../reports/unexpected_samples_log.xlsx."))
    }
  }
}
