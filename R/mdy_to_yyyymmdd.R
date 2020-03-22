#' Returns character vector of dates where entities of mm/dd/yy or
#' mm-dd-yy are converted to yyyymmdd.
#'
#' @param dates character vector of potential dates
#' @param sep character vector of length one having the character(s) to be used
#' to separate elements with each date.
#' @return date with format of yyyymmdd in character vector
#' @import stringi
#' @export
mdy_to_yyyymmdd <- function(dates, sep = "") {
  dates <- stri_replace_all_fixed(dates, "/", "-")
  dates <- sapply(dates, function(.date) {

    if (!is.na(.date) & stri_detect_fixed(.date, "-")) {
      tmp <- stri_split_fixed(.date, "-")[[1]]
      if (length(tmp) == 3) {
        tmp[1] <- stri_sub(stri_c("0", tmp[1], collapse = ""), -2, 3)
        tmp[2] <- stri_sub(stri_c("0", tmp[2], collapse = ""), -2, 3)
        if (stri_length(tmp[3]) == 2) { # Not robust code because of fixed date
          if (as.integer(tmp[3]) < 50) {
            tmp[3] <- stri_c("20", tmp[3])
          } else {
            tmp[3] <- stri_c("19", tmp[3])
          }
        }
        .date <- stri_c(tmp[3], sep, tmp[1], sep, tmp[2], collapse = "")
      }
      .date
    }
    .date
  })
  names(dates) <- NULL
  dates
}
