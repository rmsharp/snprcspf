#' Throws and error on one or more bad cage numbers are provided.
#' It terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param cage numeric vector of \code{cage}
#' column of a blood sample file.
#' @param file_name character vector of length one with the file name.
#' @param conn database connection object to the animal database
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import animalr
#' @import rmsutilityr
#' @import stringi
#' @export
test_for_cage <- function(cage, file_name, conn, run_props, run_error) {
  not_cage <- !is_location(conn, cage)
  if (any(not_cage)) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             cage locations values that are not valid locations.
             They are on the following row(s): ",
             get_and_or_list(seq_along(cage)[not_cage]),
             " with the following respective value(s): ",
             get_and_or_list(cage[not_cage]), "."))
  }
}
