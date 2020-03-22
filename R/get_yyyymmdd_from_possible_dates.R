#' Returns yyyymmdd in with optional separator between date units if
#' string looks like a date.
#'
#' @param dates character vector with possible dates
#' @param sep character string to be used to separate date units.
#' @import stringi
#' @export
get_yyyymmdd_from_possible_dates <- function(dates, sep = "-") {
  dates <- sapply(dates, function(.date) {
    #cat(stri_c(".date: ", .date, "\n"))
    if (is.na(.date)) {
      NA
    } else {
      if (stri_length(.date) >= 8 | stri_detect_regex(.date, "[-/]")) {
        if (stri_detect_regex(.date, "[-/]")) {
          if (stri_length(stri_split_regex(.date, "[-/]")[[1]][1]) == 4) {
            .date
          } else {
            mdy_to_yyyymmdd(.date, sep = sep)
          }
        } else if (stri_length(.date) < 8) {
          NA
        } else {
          stri_c(stri_sub(.date, 1, 4),
                 sep,
                 stri_sub(.date, 5, 6),
                 sep,
                 stri_sub(.date, 7, 8))
        }
      } else {
        .date
      }
    }
  })
  names(dates) <- NULL
  dates
}
