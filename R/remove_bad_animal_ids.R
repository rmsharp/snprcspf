#' Returns only good SNPRC animal Ids given a character vector of potential Ids
#'
#' @param conn database connection object
#' @param ids character vector of potential animal Ids.
#' @import animalr
#' @import rmsutilityr
#' @import RODBC
#' @export
remove_bad_animal_ids <- function(conn, ids) {
  ids <- blank_fill_ids(ids)
  bad_ids <- animalr::is_bad_animal_id(conn, ids)
  ids[ids %in% bad_ids] <- NA
  ids
}
