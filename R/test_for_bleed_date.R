#' Throws and error on one or more provided bleed dates occur on a date that
#' the animal was not alive. If the animal never existed at SNPRC, it was not
#' alive.
#' It terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param snprc_id character vector of \code{snprc_id}
#' column of a blood sample file.
#' @param bleed_date character vector ofb  bv
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
test_for_bleed_date <- function(snprc_id, bleed_date, file_name, conn,
                                run_props, run_error) {
  id_date_df <- data.frame(id = snprc_id, sample_date = bleed_date)
  bad_date <- !is_animal_alive(conn, id_date_df)
  if (any(bad_date)) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             animal Id - bleed date combinations are not possible because
             the indicated animal was not alive at the SNPRC on the bleed
             date indicated.
             They are on the following row(s): ",
             get_and_or_list(seq_along(snprc_id)[bad_date]),
             " with the following respective value(s): ",
             get_and_or_list(stri_c(snprc_id[bad_date], " on date ",
                                    bleed_date[bad_date])), "."))
  }
}
