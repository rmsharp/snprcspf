#' Returns the mean_raw_mfi_df from an Excel file (.xlsx extension)
#'
#' @param conn database connection object
#' @param file fully qualified file name of Excel file
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import readxl
#' @import rmsutilityr
#' @import stringi
#' @export
get_mean_raw_mfi_from_xlsx <- function(conn, file, run_props, run_error) {
  sheet_names <- excel_sheets(file)
  if (any(stri_detect_regex(toupper(sheet_names), "^MAC"))) {
    if (any(stri_detect_fixed(toupper(sheet_names), "MFI AVERAGE"))) {
      ## It is a "raw" (XML renamed with MAC TRACK worksheet added) Excel file
      content <- read_excel(file, sheet = "MFI AVERAGE")
      col_names <- names(content)
      col_names[1] <- "sample"
      col_names[2] <- "wells"
      names(content) <- col_names
      content <- content[-1, ]
    } else {
      stop(stri_c("This file, (", basename(file), ") looks like a file that
                should have a MFI AVERAGE worksheet, but does not.
                  The worksheets found were ",
                  get_and_or_list(sheet_names), "."))
    }
  } else if (length(sheet_names) > 3) {
    stop(stri_c("This file, (", basename(file), ") looks like a file that
                should have a MAC TRACK worksheet, but does not.
                The worksheets found were ",
                get_and_or_list(sheet_names), "."))
  } else {# It is a Report style formated file
    content <- read_excel(file)
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    #results <- get_result_tables(content)
    # repeats are already averaged in Excel
    content <- get_raw_mfi_tables(content)
    col_names <- as.character(content[1, ])
    col_names[1] <- "sample"
    col_names[2] <- "wells"
    col_names <- col_names[1:(which(col_names %in% c(NA, "NA"))[1] - 1)]
    content <- content[2:nrow(content), ]
    if (any(is.na(content[ , 1]))) {
      n_rows <- which(is.na(content[ , 1]))[1] - 1
    } else {
      n_rows <- length(as.character(content[ , 1]))
    }

    content <- content[2:n_rows, 1:length(col_names)]
    names(content) <- col_names
  }
  content <- content[!is.na(content$sample), ]
  get_raw_mfi_df(conn, content, basename(file), run_props, run_error)
}
