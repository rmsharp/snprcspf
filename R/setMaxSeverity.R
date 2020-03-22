#' Returns integer value of error level (0:NONE, 1:WARN, 2:ERROR) used to set
#' run_error$level.
#'
#' If the new level is 2 then 2 is returned.
#' If the new level is the same as the current level and not 2, 0 is returned.
#' If the new level is greater than the current level and not 2 (that is if it
#' is 1) and the current severity level is not "ERROR", the new level (1) is
#' returned.
#' If the new level is 1 and the current severity level is "ERROR", 0 is
#' returned.

#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location") defined when a Run Properties File provided
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @param level integer value being used to reset run_error$level
#' @export
setMaxSeverity <- function(run_props, run_error, level) {
  max(run_error$level, level)
  # value <- 0
  #
  # # Don't display warnings if severityLevel set to ERROR
  # if (level == 2) {
  #   value <- 2
  # } else if (getRunPropertyValue(run_props, "severityLevel") != "ERROR" &&
  #            level > run_error$level) {
  #   value <- level
  # }
  # value
}
