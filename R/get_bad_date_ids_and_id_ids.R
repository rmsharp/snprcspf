#' Returns a list with two character vectors containing the items in the
#' vector dates that are not Ids and the items in ids that are not Ids.
#'
#' @param conn database connection object
#' @param ids character vector of possible Ids (or dates)
#' @param dates character vector of possible Ids (or dates)
#' @import animalr
#' @import RODBC
#' @import stringi
get_bad_date_ids_and_id_ids <- function(conn, ids, dates) {
  bad_ids <-
    stri_trim_both(suppressWarnings(is_bad_animal_id(conn, ids[!is.na(ids)])))
  bad_date_ids <-
    stri_trim_both(suppressWarnings(is_bad_animal_id(conn,
                                                     dates[!is.na(dates)])))
  list(bad_ids = bad_ids, bad_date_ids = bad_date_ids)
}
