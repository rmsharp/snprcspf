#' Returns dataframe with true animal Ids (Id) and date strings (Date) from
#' Animal.ID column.
#'
#' @param animal_id_col character vector containing the composite descriptor
#' that has the animal Id and sample date. I may also have an 'R' indicating
#' that the sample is a repeated sample.
#' @import stringi
#' @export
get_id_date_repeated <- function(animal_id_col) {
  repeated <- stri_detect_fixed(toupper(animal_id_col), "R")
  id_dates <- stri_trim_both(unlist(stri_split_fixed(animal_id_col, "-")))
  id_dates <- id_dates[toupper(id_dates) != "R"]
  is_id_col <- stri_length(id_dates) <= 6
  dates <- id_dates[!is_id_col]
  long_dates <- dates[stri_detect_fixed(dates, "/")]
  dates[stri_detect_fixed(dates, "/")] <-
    stri_c(stri_sub(long_dates, 7, 10),
           stri_sub(long_dates, 1, 2),
           stri_sub(long_dates, 4, 5))
  data.frame(Id = id_dates[is_id_col], Date = dates,
             Repeated = ifelse(repeated, "Y", "N"), stringsAsFactors = FALSE)
}
