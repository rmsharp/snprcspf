#' Returns character vector with the raw data (raw mfi) table
#'
#' @param content character vector with lines from the Excel file worksheet.
#' @export
get_raw_mfi_tables <- function(content) {
  possible_id_headers <- c("groupid", "animalid", "groupname")

  for (i in 1:nrow(content)) {
    if (tolower(content[[i, 1]]) %in% possible_id_headers)
      break
  }
  start <- i - 1
  for (i in start:nrow(content)) {
    if (i > 4 & tolower(content[[i, 1]]) %in% possible_id_headers)
      break
  }
  end <- i - 2
  data.frame(content[start:end, ], stringsAsFactors = FALSE)
}
