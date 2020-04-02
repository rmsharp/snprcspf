#' Returns report date in yyyy-mm-dd format as derived from the file names.
#'
#' @param filenames character vector with one or more file names. The
#' file names of the Excel files have embedded report dates.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#'
#' @import stringi
#' @export
get_report_date_from_filename <- function(filenames, run_props, run_error) {
  orig_dates <- rep(NA, length(filenames))
  dates <- orig_dates
  tmp_names <-
    basename(filenames)[!stri_detect_fixed(tolower(basename(filenames)),
                                           "spfcolony")]
  if (any(stri_detect_regex(tolower(tmp_names), "plate [0-9]+"))) {
    tmp_dates <- sapply(stri_split_regex(tolower(tmp_names), "plate [0-9]+"),
                        function(x) {x[[2]]})
    tmp_dates <-
      stri_sub(tmp_dates,
               stri_locate_first_regex(tmp_dates,
                                       pattern = "[0-9]{2}-[0-9]{2}-[0-9]{2,4}"))
    dates[!stri_detect_fixed(tolower(basename(filenames)), "spfcolony")] <-
      tmp_dates
    dates <- mdy_to_yyyymmdd(dates, sep = "-")
    if (any(is.na(ymd(dates) & is.na(orig_dates)))) {
      # errMessage <- stri_c("The file name date is malformed: ",
      #                      get_and_or_list(tmp_dates[is.na(ymd(dates))]))
      triggerError(run_props, run_error, "The file name date is malformed")
    }
    dates
  } else {
    triggerError(run_props, run_error,
                 "The file name is missing the 'plate #' section.")
  }
  dates
}
