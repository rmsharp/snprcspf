#' Returns a dataframe with four columns ("name", "value",
#' "java_data_type", and "location"), which is defined when a Run Properties
#' File provided by the system is read.
#'
#' @param properties_file character string with the file name provided by the
#' system as a replacement value for the macro \emph{${runInfo}}
#' @export
readRunPropertiesFile <- function(properties_file) {
  utils::read.table(properties_file, header = FALSE, sep = "\t",
                    stringsAsFactors = FALSE,
                    col.names = c("name", "value", "java_data_type", "location"),
                    fill = TRUE)
}
