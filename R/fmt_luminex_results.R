#' Conditionally formats a worksheet based on data within the dataframe as a
#' side effect.
#'
#' @return Excel file name of file that has been formatted to highlight
#' non-negative results in the sheet indicated. This highlights "I" and
#' "P" results
#'
#' @param excel_file character vector with the name of the Excel file
#' @param df_list List of dataframes used as the source data for formating and
#' for writing the Excel file worksheet. The two (data controlling formating
#' and data written)  may not be the same.
#' @param fmtIndexNum Index into the list of dataframes (\code{df_list} used to
#' guide format highlighting.
#' @param wrkSheetIndex Index into the list of dataframes (\code{df_list} used
#' to write the data into the worksheet.
#' @param sheet_name name of the worksheet to be written and highlighted.
#' @param low_positive_controls_df dataframe with the low positive controls
#'
#' @importFrom grDevices rgb
#' @importFrom stats complete.cases
#' @importFrom openxlsx loadWorkbook createStyle writeData addStyle saveWorkbook
#' @export
fmt_luminex_results <- function(excel_file, df_list, fmtIndexNum, wrkSheetIndex,
                                sheet_name = "final_result",
                                low_positive_controls_df) {
  fmtData <- df_list[[fmtIndexNum]]
  wrkSheetData <- df_list[[wrkSheetIndex]]
  ## format cells with highlighting, text wrapping, grey header, and column
  ## specific borders
  wb <- loadWorkbook(excel_file)
  highlightRed <- rgb(red = 255, green = 0, blue = 0,
                      maxColorValue = 255)
  highlightBlue <- rgb(red = 0, green = 191, blue = 255,
                       maxColorValue = 255)
  highlightWheat <- rgb(red = 245, green = 222, blue = 179,
                        maxColorValue = 255)
  fontYellow <- rgb(red = 255, green = 255, blue = 0, maxColorValue = 255)
  cs_positive <- createStyle(fgFill = highlightRed, fontColour = fontYellow,
                             wrapText = TRUE)
  cs_indeterminate <- createStyle(fgFill = highlightBlue, wrapText = TRUE)
  cs_to_repeat <- createStyle(fgFill = highlightWheat, wrapText = TRUE) # low positive control wells
  ## set-rows
  cs_header <- createStyle(fgFill = "grey85", wrapText = TRUE)
  cs_border_header <- createStyle(fgFill = "grey85", wrapText = TRUE)

  ## make-matrices
  ## Include row offset for column label in Excel sheets
  row_number <- matrix(data = rep(1:nrow(fmtData), each = ncol(fmtData)),
                        nrow = nrow(fmtData),
                        ncol = ncol(fmtData), byrow = TRUE) + 1L
  col_number <- matrix(data = rep(1:ncol(fmtData), each = nrow(fmtData)),
                        nrow = nrow(fmtData),
                        ncol = ncol(fmtData))

  positive <- data.frame(row = row_number[fmtData == "P"],
                        col = col_number[fmtData == "P"])
  indeterminate <- data.frame(row = row_number[fmtData == "I"],
                             col = col_number[fmtData == "I"])
  ## Include row offset for column label in Excel sheets
  well_rows <- ((1:nrow(fmtData)) + 1L)[fmtData[ , "wells"] %in%
                              low_positive_controls_df$wells]
  to_repeat <- data.frame(
    row = rep(well_rows, ncol(fmtData)),
    col = rep(1:ncol(fmtData), each = length(well_rows)))

  positive <- positive[complete.cases(positive), ]
  indeterminate <- indeterminate[complete.cases(indeterminate), ]
  ## set-style
  writeData(
    wb,
    sheet = sheet_name,
    x = wrkSheetData,
    borders = "none",
    borderStyle = "thin"
  )
  if (nrow(positive) > 0) {
     addStyle(wb, sheet = sheet_name,
              style = cs_positive, rows = positive$row,
              cols = positive$col, gridExpand = FALSE)
  }
  if (nrow(indeterminate) > 0) {
     addStyle(wb, sheet = sheet_name,
              style = cs_indeterminate, rows = indeterminate$row,
              cols = indeterminate$col, gridExpand = FALSE)
  }
  if (nrow(to_repeat) > 0) {
     addStyle(wb, sheet = sheet_name,
              style = cs_to_repeat, rows = to_repeat$row,
              cols = to_repeat$col, gridExpand = FALSE)
  }
  addStyle(wb, sheet = sheet_name,
          style = cs_header, rows = 1,
          cols = seq_along(names(fmtData)), gridExpand = FALSE)
  ## Commented out as currently redundant since the styles match
  ##addStyle(wb, sheet = sheet_name,
  ##         style = cs_border_header, rows = 1,
  ##         cols = seq_along(names(fmtData)), gridExpand = TRUE)
  saveWorkbook(wb, file = excel_file, overwrite = TRUE)

  excel_file
}