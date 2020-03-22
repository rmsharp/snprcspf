#' Read blood or dna sample file
#'
#' @return dataframe with animal Ids, cage location, bleed date, whether not a
#' blood sample was expected, whether or not a sample was received, and the
#' Pool Id number if present for a single sample file.
#'
#' @param file_name fully qualified file name of Excel file
#' @param conn database connection object.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import readxl
#' @import stringi
#' @export
read_sample_file <- function(file_name, conn, run_props, run_error) {
  sample_df <- data.frame(file_name = character(0),
                          snprc_id = character(0),
                          cage = numeric(0),
                          bleed_date = character(0),
                          blood_expected = character(0),
                          blood_received = character(0),
                          pool_id = character(0))
  tmp_sample_df <- read_excel(file_name, col_names = TRUE)
  tmp_sample_df <- Filter(function(x)!all(is.na(x)), tmp_sample_df)
  sample_col_names <- names(sample_df)
  if (any("pool_id" %in% setdiff(sample_col_names, names(tmp_sample_df)))) {
    pooled_samples <- FALSE
  } else {
    pooled_samples <- TRUE
  }
  if (length(tmp_sample_df) == length(sample_col_names)) {
    tmp_sample_df <- tmp_sample_df[ , -1] # remove unused row number
  } else if (pooled_samples &
             !length(tmp_sample_df) == (length(sample_col_names) - 1)) {
    triggerError(stri_c("File: ", file_name,
                        " does not have the correct number of columns. ",
                        "It should have the following columns: ",
                        get_and_or_list(sample_col_names[-1]), "."))
  } else if (!pooled_samples &
             !length(tmp_sample_df) == (length(sample_df) - 2)) {
    triggerError(
      stri_c("File: ", file_name,
             " does not have the correct number of columns. ",
             "It should have the following columns: ",
             get_and_or_list(sample_col_names[
               c(-1, -length(sample_col_names))]), "."))
  }
  if (!pooled_samples) {
    tmp_sample_df$pool_id <- NA
  }
  names(tmp_sample_df) <- sample_col_names[-1] # does not have file

  tmp_sample_df <- data.frame(tmp_sample_df)
  tmp_sample_df <-
    tmp_sample_df[!is.na(tmp_sample_df$snprc_id), ]
  tmp_sample_df$cage <- as.numeric(tmp_sample_df$cage)
  tmp_sample_df$bleed_date <-
    stri_datetime_format(
      stri_datetime_parse(tmp_sample_df$bleed_date, format = "uuuuMMdd"),
      format = "uuuu-MM-dd")
  tmp_sample_df$snprc_id <- blank_fill_ids(tmp_sample_df$snprc_id)

  test_for_yes_no(tmp_sample_df$blood_expected, file_name, "blood_expected",
                  run_props, run_error)
  test_for_yes_no(tmp_sample_df$blood_received, file_name, "blood_received",
                  run_props, run_error)
  test_for_snprc_id(tmp_sample_df$snprc_id, file_name, conn, run_props,
                    run_error)
  test_for_cage(tmp_sample_df$cage, file_name, conn, run_props, run_error)
  test_for_bleed_date(tmp_sample_df$snprc_id, tmp_sample_df$bleed_date,
                      file_name, conn, run_props, run_error)
  tmp_sample_df$blood_expected[
    tolower(tmp_sample_df$blood_expected) == "yes"] <- "Y"
  tmp_sample_df$blood_expected[
    tolower(tmp_sample_df$blood_expected) == "no"] <- "N"
  tmp_sample_df$blood_received[
    tolower(tmp_sample_df$blood_received) == "yes"] <- "Y"
  tmp_sample_df$blood_received[
    tolower(tmp_sample_df$blood_received) == "no"] <- "N"
  tmp_sample_df$pooled <- ifelse(stri_detect_regex(tmp_sample_df$pool_id, "^V"),
                                 "Y", "N")
  tmp_sample_df$pooled[is.na(tmp_sample_df$pool_id)] <- "N"
  tmp_sample_df
}
