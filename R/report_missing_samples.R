#' Creates an Excel file with a list of samples that were expected but not
#' received.
#'
#'
#' This needs testing.
#'
#' @param sample_df dataframe from sample file
#' @import rmsutilityr
#' @import stringi
#' @export
report_missing_samples <- function(sample_df) {
  if (!all(is.na(sample_df$snprc_id[sample_df$blood_received == "no"]))) {
    missing_samples <-
      sample_df[sample_df$blood_received == "no" &
                  sample_df$blood_expected == "yes", ]
    if (nrow(missing_samples) > 0) {
      missing_samples$cage <- round(missing_samples$cage, 2)
      create_wkbk(file = "../reports/missing_samples_log.xlsx",
                  list(missing_samples), "missing samples")
      warning(stri_c("There are one or more blood ",
                     "samples that were expected were not received. ",
                     "They are listed in ",
                     "../reports/missing_samples_log.xlsx."))
    }

  }
}
