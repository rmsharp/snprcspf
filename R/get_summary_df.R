#' Returns dataframe with summary results from the results section of the Excel
#' file.
#'
#' @param results character vector with line from the Excel file worksheet
#' having only the result table records
#' @export
get_summary_df <- function(results) {
  col_names <- as.character(results[1, ])
  len <- length(col_names)
  col_names <- col_names[(which(col_names %in% NA)[1] + 1):length(col_names)]
  col_names <-
    col_names[(which(!col_names %in% c(NA, "NA"))[1]):length(col_names)]
  offset <- len - length(col_names)
  col_names <- col_names[1:(which(col_names %in% c(NA, "NA"))[1] - 1)]
  col_names <- remove_string(col_names, "SIV mac")
  if (any(as.character(results[ , 1]) %in% NA)) {
    n_rows <- which(as.character(results[ , 1]) %in% c(NA, "NA"))[1] - 1
  } else {
    n_rows <- length(as.character(results[ , 1]))
  }
  summary_df <- results[2:n_rows, (1:length(col_names)) + offset]
  names(summary_df) <- col_names
  summary_df
}
