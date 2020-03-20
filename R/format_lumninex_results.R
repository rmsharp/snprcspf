#' Returns Excel file name of file that has been formatted to highlight
#' non-negative results in the sheet indicated.
#'
#' This highlights "I" and "P" results
#' @param excel_file character vector with the name of the Excel file
#' @param .df dataframe being used to guide highlighting. It is the same
#' one as was used to make the worksheet.
#' @param sheet_name name of the worksheet to highlight.
#' @param low_positive_controls_df dataframe with the low positive controls
#' @importFrom stats complete.cases
#' @import openxlsx
#' @export
format_luminex_results <- function(excel_file, .df,
                                  sheet_name = "final_result",
                                  low_positive_controls_df) {
 ## set-rows
 ## format cells with highlighting, text wrapping, grey header, and column
 ## specific borders
 wb <- createWorkbook(excel_file)
 cs_positive <- createCellStyle(wb)
 cs_indeterminate <- createCellStyle(wb)
 cs_to_repeat <- createCellStyle(wb) # low positive control wells
 cs_header <- createCellStyle(wb)
 cs_border_header <- createCellStyle(wb)
 ## set-wrap
 setWrapText(cs_positive, wrap = TRUE)
 setWrapText(cs_indeterminate, wrap = TRUE)
 setWrapText(cs_to_repeat, wrap = TRUE)
 setWrapText(cs_header, wrap = TRUE)
 setWrapText(cs_border_header, wrap = TRUE)
 ## set-fill
 setFillPattern(cs_positive, fill = XLC$FILL.SOLID_FOREGROUND)
 setFillForegroundColor(cs_positive, color = XLC$COLOR.ROSE)
 setFillPattern(cs_indeterminate, fill = XLC$FILL.SOLID_FOREGROUND)
 setFillForegroundColor(cs_indeterminate, color = XLC$COLOR.CORNFLOWER_BLUE)
 setFillPattern(cs_to_repeat, fill = XLC$FILL.SOLID_FOREGROUND)
 setFillForegroundColor(cs_to_repeat, color = XLC$COLOR.LEMON_CHIFFON)
 setFillPattern(cs_header, fill = XLC$FILL.SOLID_FOREGROUND)
 setFillPattern(cs_border_header, fill = XLC$FILL.SOLID_FOREGROUND)
 setFillForegroundColor(cs_header, color = XLC$COLOR.GREY_25_PERCENT)
 setFillForegroundColor(cs_border_header, color = XLC$COLOR.GREY_25_PERCENT)
 ## make-matrices
 ## Include row offset for column label in Excel sheets
 positive_row <- matrix(data = rep(1:nrow(.df), each = ncol(.df)),
                        nrow = nrow(.df),
                        ncol = ncol(.df), byrow = TRUE) + 1L
 positive_col <- matrix(data = rep(1:ncol(.df), each = nrow(.df)),
                        nrow = nrow(.df),
                        ncol = ncol(.df))
 ##  positive_row <- positive_row[!is.na(.df)]
 ##  positive_col <- positive_col[!is.na(.df)]
 indeterminate_row <- positive_row
 indeterminate_col <- positive_col
 to_repeat_row <- positive_row
 to_repeat_col <- positive_col
 positive <- data.frame(row = positive_row[.df == "P"],
                        col = positive_col[.df == "P"])
 indeterminate <- data.frame(row = indeterminate_row[.df == "I"],
                             col = indeterminate_col[.df == "I"])
 ## Include row offset for column label in Excel sheets
 well_rows <- ((1:nrow(.df)) + 1L)[.df[ , "wells"] %in%
                              low_positive_controls_df$wells]
 to_repeat <- data.frame(
   row = rep(well_rows, ncol(.df)),
   col = rep(1:ncol(.df), each = length(well_rows)))
 positive <- positive[complete.cases(positive), ]
 indeterminate <- indeterminate[complete.cases(indeterminate), ]
 ## set-style
 if (nrow(positive) > 0) {
   setCellStyle(wb, sheet = sheet_name, row = positive$row, col = positive$col,
                cellstyle = cs_positive)
 }
 if (nrow(indeterminate) > 0) {
   setCellStyle(wb, sheet = sheet_name, row = indeterminate$row,
              col = indeterminate$col,
              cellstyle = cs_indeterminate)
 }
 if (nrow(to_repeat) > 0) {
   setCellStyle(wb, sheet = sheet_name, row = to_repeat$row,
                col = to_repeat$col,
                cellstyle = cs_to_repeat)
 }
 setCellStyle(wb, sheet = sheet_name, row = 1, col = seq_along(names(.df)),
              cellstyle = cs_header)
 ## setCellStyle(wb, sheet = sheet_name, row = positive$row,
 ##              col = positive$col,
 ##              cellstyle = cs_border_positive)
 ## setCellStyle(wb, sheet = sheet_name, row = indeterminate$row,
 ##              col = indeterminate$col,
 ##              cellstyle = cs_border_indeterminate)
 setCellStyle(wb, sheet = sheet_name, row = 1, col = seq_along(names(.df)),
              cellstyle = cs_border_header)
 ## save-wb
 saveWorkbook(wb)
 excel_file
}
