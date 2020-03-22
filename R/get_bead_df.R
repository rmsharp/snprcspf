#' Get bead call multipliers from results section of Excel SPF Report file.
#'
#' Takes a dataframe with the results section of the data in an Excel
#' SPF Report file, reads in the cuttoff values from the sheet, and creates
#' a dataframe with the bead call results to use as multipliers.
#'
#' @return dataframe with bead call results from the results section of the
#' Excel file.
#'
#' @param results character vector with line from the Excel file worksheet
#' having only the result table records
#' @import stringi
#' @export
get_bead_df <- function(results) {
  col_names <- as.character(results[1, ])
  col_names <- col_names[1:(which(col_names %in% c(NA, "NA"))[1] - 1)]
  if (any(as.character(results[ , 1]) %in% c(NA, "NA"))) {
    n_rows <- which(as.character(results[ , 1]) %in% c(NA, "NA"))[1] - 1
  } else {
    n_rows <- length(as.character(results[ , 1]))
  }
  bead_df <- results[2:n_rows, 1:length(col_names)]
  col_names[stri_detect_fixed(tolower(col_names), "animal")] <- "Animal.ID"
  names(bead_df) <- col_names
  bead_df
}
