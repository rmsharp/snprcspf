#' Returns full name with path of transformed output file.
#'
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @export
getTransformedOutputFile <- function(run_props) {
  value <- NA
  if (any(run_props$name == "runDataFile")) {
    value <- run_props$location[run_props$name == "runDataFile"]
    # return NA for an empty string
    if (nchar(value) == 0) {
      value <- NA
    }
  }
  value
}
