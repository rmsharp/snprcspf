#' Returns the date from a sample column if it exist else NA.
#'
#' @param conn database connection object
#' @param samples character vector with one or more sample dates
#' @import stringi
#' @export
get_date_from_sample <- function(conn, samples) {
  possible <- get_possible_ids_dates(samples)
  bad <- get_bad_date_ids_and_id_ids(conn, possible$ids, possible$dates)
  date_not_id <- possible$dates %in% bad$bad_date_ids
  id_not_id <- possible$ids %in% bad$bad_ids
  if (any(date_not_id & id_not_id & !is.na(possible$dates) &
          !is.na(possible$ids) &
          stri_length(possible$dates) > 6 & stri_length(possible$ids) > 6))
    stop(stri_c("samples may have multiple valid dates: ",
                get_and_or_list(samples[date_not_id & id_not_id])))
  possible$dates[id_not_id] <- possible$ids[id_not_id]
  possible$dates[stri_length(possible$dates) < 8] <- NA
  if (any(is.na(possible$dates)) &
      any(!stri_detect_fixed(samples[is.na(possible$dates)], "CNT"))) {
    warning(stri_c(
      "The following sample labels do not have recognizable dates: ",
      get_and_or_list(samples[is.na(possible$dates) &
                                !stri_detect_fixed(samples, "CNT")])))
  }
  possible$dates
}
