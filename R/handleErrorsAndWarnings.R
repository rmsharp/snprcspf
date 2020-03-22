#' Returns NULL: does not return -- writes the maximumSeverity level to the
#' transformRunProperties file and the error message to the error.html file.
#'
#' LabKey server will read these files after execution to determine if an error
#' or warning occurred and handle it appropriately
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import stringi
#' @export
handleErrorsAndWarnings <- function(run_props, run_error) {
  if (run_error$level > 0) {
    fileConn <- file(getRunPropertyValue(run_props,
                                         "transformedRunPropertiesFile"))
    # if (!isOpen(fileConn))
    #   stop(stri_c("Failed to open ", getRunPropertyValue(run_props,
    #                                                      "transformedRunPropertiesFile")))
    if (run_error$level == 1) {
      writeLines(c(stri_c("maximumSeverity ", "WARN ", sep = "\t")), fileConn)
    }
    else {
      writeLines(c(stri_c("maximumSeverity ", "ERROR ", sep = "\t")), fileConn)
    }
    close(fileConn)

    # This file gets read and displayed directly as warnings or errors,
    # depending on maximumSeverity level.
    if (!is.null(run_error$msg)) {
      fileConn <- file("errors.html", open = "a")
      writeLines(run_error$msg, fileConn)
      close(fileConn)
    }
    quit("no")
  }
}
