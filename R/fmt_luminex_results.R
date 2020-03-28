#' Conditionally formats a worksheet based on data within the dataframe as a
#' side effect.
#'
#' @return Excel file name of file that has been formatted to highlight
#' non-negative results in the sheet indicated. This highlights "I" and
#' "P" results
#'
#' @param excel_file character vector with the name of the Excel file
#' @param dFrame dataframe being used to guide highlighting. It is the same
#' one as was used to make the worksheet.
#' @param sheet_name name of the worksheet to highlight.
#' @param low_positive_controls_df dataframe with the low positive controls
#'
#' @importFrom grDevices rgb
#' @importFrom stats complete.cases
#' @importFrom openxlsx loadWorkbook createStyle writeData addStyle saveWorkbook
#' @export
fmt_luminex_results <- function(excel_file, dFrame,
                                sheet_name = "final_result",
                                low_positive_controls_df) {
  ## format cells with highlighting, text wrapping, grey header, and column
  ## specific borders
  wb <- loadWorkbook(excel_file)
  highlightRed <- rgb(red = 255, green = 150, blue = 150,
                      maxColorValue = 255)
  highlightBlue <- rgb(red = 0, green = 191, blue = 255,
                       maxColorValue = 255)
  highlightWheat <- rgb(red = 245, green = 222, blue = 179,
                        maxColorValue = 255)
  cs_positive <- createStyle(fgFill = highlightRed, wrapText = TRUE)
  cs_indeterminate <- createStyle(fgFill = highlightBlue, wrapText = TRUE)
  cs_to_repeat <- createStyle(fgFill = highlightWheat, wrapText = TRUE) # low positive control wells
  ## set-rows
  cs_header <- createStyle(fgFill = "grey85", wrapText = TRUE)
  cs_border_header <- createStyle(fgFill = "grey85", wrapText = TRUE)

  ## set-fill
  ## setFillPattern(cs_positive, fill = XLC$FILL.SOLID_FOREGROUND)
  ## setFillPattern(cs_indeterminate, fill = XLC$FILL.SOLID_FOREGROUND)
  ## setFillPattern(cs_to_repeat, fill = XLC$FILL.SOLID_FOREGROUND)
  ## setFillPattern(cs_header, fill = XLC$FILL.SOLID_FOREGROUND)
  ## setFillPattern(cs_border_header, fill = XLC$FILL.SOLID_FOREGROUND)

  ## make-matrices
  ## Include row offset for column label in Excel sheets
  positive_row <- matrix(data = rep(1:nrow(dFrame), each = ncol(dFrame)),
                        nrow = nrow(dFrame),
                        ncol = ncol(dFrame), byrow = TRUE) + 1L
  positive_col <- matrix(data = rep(1:ncol(dFrame), each = nrow(dFrame)),
                        nrow = nrow(dFrame),
                        ncol = ncol(dFrame))

  indeterminate_row <- positive_row
  indeterminate_col <- positive_col
  to_repeat_row <- positive_row
  to_repeat_col <- positive_col
  positive <- data.frame(row = positive_row[dFrame == "P"],
                        col = positive_col[dFrame == "P"])
  indeterminate <- data.frame(row = indeterminate_row[dFrame == "I"],
                             col = indeterminate_col[dFrame == "I"])
  ## Include row offset for column label in Excel sheets
  well_rows <- ((1:nrow(dFrame)) + 1L)[dFrame[ , "wells"] %in%
                              low_positive_controls_df$wells]
  to_repeat <- data.frame(
    row = rep(well_rows, ncol(dFrame)),
    col = rep(1:ncol(dFrame), each = length(well_rows)))

  positive <- positive[complete.cases(positive), ]
  indeterminate <- indeterminate[complete.cases(indeterminate), ]
  ## set-style
  writeData(wb, sheet = sheet_name, x = dFrame, borders = "none",
           borderStyle = "thin")
  if (nrow(positive) > 0) {
     addStyle(wb, sheet = sheet_name,
              style = cs_positive, rows = positive$row,
              cols = positive$col, gridExpand = TRUE)
  }
  if (nrow(indeterminate) > 0) {
     addStyle(wb, sheet = sheet_name,
              style = cs_indeterminate, rows = indeterminate$row,
              cols = indeterminate$col, gridExpand = TRUE)
  }
  if (nrow(to_repeat) > 0) {
     addStyle(wb, sheet = sheet_name,
              style = cs_to_repeat, rows = to_repeat$row,
              cols = to_repeat$col, gridExpand = TRUE)
  }
  addStyle(wb, sheet = sheet_name,
          style = cs_header, rows = 1,
          cols = seq_along(names(dFrame)), gridExpand = TRUE)
  ## Commented out as currently redundant since the styles match
  ##addStyle(wb, sheet = sheet_name,
  ##         style = cs_border_header, rows = 1,
  ##         cols = seq_along(names(dFrame)), gridExpand = TRUE)
  ## save-wb
  saveWorkbook(wb, file = excel_file, overwrite = TRUE)

  excel_file
}