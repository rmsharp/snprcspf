#' Returns the first integer value from a sample column if it exist else NA.
#'
#' @param conn database connection object
#' @param samples character vector with one or more sample Ids
#' @import stringi
#' @export
get_id_from_sample <- function(conn, samples) {
  possible <- get_possible_ids_dates(samples)
  bad <- get_bad_date_ids_and_id_ids(conn, possible$ids, possible$dates)
  date_is_id <- !possible$dates %in% bad$bad_date_ids
  id_is_id <- !possible$ids %in% bad$bad_ids
  if (any(date_is_id & id_is_id & !is.na(possible$dates) &
          !is.na(possible$ids)))
    stop(stri_c("samples has two valid animal Ids: ",
                get_and_or_list(samples[date_is_id & id_is_id])))
  possible$ids[!is.na(possible$dates) & date_is_id & !id_is_id] <-
    possible$dates[!is.na(possible$dates) & date_is_id]
  possible$ids[possible$ids %in% bad$bad_ids] <- NA
  possible$ids
}
