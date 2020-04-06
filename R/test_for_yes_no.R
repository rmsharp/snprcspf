#' Throws and error on bad input for the \code{expected_blood}
#' or \code{received_blood} column
#' that terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param expected character vector of values from the \code{expected_blood}
#' or \code{received_blood} column of a blood sample file.
#' @param file_name character vector of length one with the file name.
#' @param col_name character vector of length one with column name.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @importFrom rmsutilityr get_and_or_list
#' @importFrom stringi stri_c
#' @export
test_for_yes_no <- function(expected, file_name, col_name, run_props,
                            run_error) {
  if (any(!tolower(expected) %in% c("yes", "no"))) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             '", col_name, "' values that are not either 'yes' or
             'no'. They are on the following row(s): ",
             get_and_or_list(seq_along(expected)[
               !tolower(expected) %in% c("yes", "no")]),
             " with the following respective value(s): ",
             get_and_or_list(expected[
               !tolower(expected) %in% c("yes", "no")]), "."))
  }
}
