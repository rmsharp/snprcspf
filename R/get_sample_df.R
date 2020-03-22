#' Returns dataframe with animal Ids, cage location, bleed date, whether not a
#' blood sample was expected, whether or not a sample was received, and the
#' Pool Id number if present.
#'
#' @param all_files charcter vector of all the files to process.
#' @param conn database connection object.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import stringi
#' @export
get_sample_df <- function(all_files, conn, run_props, run_error) {
  sample_df <- data.frame(file_name = character(0),
                          snprc_id = character(0),
                          cage = numeric(0),
                          bleed_date = character(0),
                          blood_expected = character(0),
                          blood_received = character(0),
                          pool_id = character(0))

  #total_number_of_files <- length(all_files)
  file_names <- basename(all_files)
  paths <- dirname(all_files)
  for (i in seq_along(all_files)) {
    file_name <- file_names[i]
    tmp_sample_df <- read_sample_file(stri_c(paths[i], "/", file_name),
                                      conn, run_props, run_error)
    sample_df <-
      rbind(sample_df,
            data.frame(file_name = rep(file_name, nrow(tmp_sample_df)),
                       tmp_sample_df))
  }
  sample_df[order(sample_df$file_name,
                  sample_df$bleed_date,
                  sample_df$cage,
                  sample_df$snprc_id), ]
}
