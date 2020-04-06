#' Throws and error on one or more bad animal Ids.
#' It terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param snprc_id character vector of \code{snprc_id}
#' @param file_name character vector of length one with the file name.
#' @param conn database connection object to the animal database
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @importFrom animalr is_primate_id
#' @importFrom rmsutilityr get_and_or_list
#' @importFrom stringi stri_c
#' @export
test_for_snprc_id <- function(snprc_id, file_name, conn, run_props, run_error) {
  not_primate <- !is_primate_id(conn, snprc_id)
  if (any(not_primate)) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             snprc_id values that are not primates.
             They are on the following row(s): ",
             get_and_or_list(seq_along(snprc_id)[not_primate]),
             " with the following respective value(s): ",
             get_and_or_list(snprc_id[not_primate]), "."))
  }
}
