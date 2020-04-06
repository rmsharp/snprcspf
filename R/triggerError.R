#' Returns NULL: does not return -- triggers error and stops execution
#'
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @param msg character string containing the warning message
#' @importFrom stringi stri_c
#' @export
triggerError <- function(run_props, run_error, msg) {
  run_error$level <- setMaxSeverity(run_props, run_error, level = 2)
  if (length(run_error$msg) == 0) {
    run_error$msg <- stri_c(msg, " Error level = ",
                            run_error$level)
  } else {
    run_error$msg <- stri_c(run_error$msg, "<br>", msg, " Error level = ",
                            run_error$level)
  }
  handleErrorsAndWarnings(run_props, run_error)
}
