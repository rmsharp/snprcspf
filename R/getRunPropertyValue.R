#' Returns value of run property if one is available else NA is provided.
#'
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param colName character vector of length 1 with the name of the property
#' whose value is sought.
#' @export
getRunPropertyValue <- function(run_props, colName) {
  value <- NA
  if (any(run_props$name == colName)) {
    value <- run_props$value[run_props$name == colName]
    # return NA for an empty string
    if (nchar(value) == 0) {
      value <- NA
    }
  }
  value
}
