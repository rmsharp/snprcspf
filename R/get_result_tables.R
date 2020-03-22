#' Returns character vector with the result tables
#'
#' @param content character vector with lines from the Excel file worksheet.
#' @export
get_result_tables <- function(content) {
  possible_id_headers <- c("groupid", "animalid")

  for (i in 20:nrow(content)) {
    if (tolower(content[[i, 1]]) %in% possible_id_headers)
      break
  }
  start <- i
  for (i in start:nrow(content)) {
    if (is.na(content[[i, 1]]))
      break
  }
  end <- i - 1
  data.frame(content[start:end, ], stringsAsFactors = FALSE)
}
