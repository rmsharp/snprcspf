library(snprcspf)
library(stringi)
library(rmsutilityr)

prefix <- stri_c(stri_replace_all_fixed(stri_replace_all_fixed(
  now(), " ", "_"), ":", "-"), "_")
input_file <- "2020-03-21_12-45-43_SPF_Colony_Testing_Plate_166_03-06-20.xlsx"
file_no_ext <- stri_sub(basename(input_file), 1,
                        stri_locate_last_fixed(basename(input_file),
                                               pattern = ".")[[1]] - 1)
excel_file <- stri_c(prefix, file_no_ext, ".xlsx")
dfs <- snprcspf::plate166_dfs
dfs$w_raw_mfi_df$file <-
  "2020-03-21_12-45-43_SPF_Colony_Testing_Plate_166_03-06-20.xlsx"
dfs$w_mfi_df$file <- dfs$w_raw_mfi_df$file
dfs$w_d_mfi_df$file <- dfs$w_raw_mfi_df$file
dfs$w_r_mfi_df$file <- dfs$w_raw_mfi_df$file
dfs$w_combined_df$file_name <- dfs$w_raw_mfi_df$file

low_positive_controls_df <- data.frame()
w_raw_mfi_df <- dfs$w_mean_raw_mfi_df
w_mfi_df <- dfs$w_mfi_df
w_d_mfi_df <- dfs$w_d_mfi_df
w_r_mfi_df <- dfs$w_r_mfi_df
w_combined_df <- dfs$w_combined_df
df_list <- list(
  w_raw_mfi_df,
  w_mfi_df,
  w_d_mfi_df,
  w_r_mfi_df,
  w_combined_df)
sheetnames <- c(
  "raw_mfi",
  "minus_background",
  "normalized",
  "antigen_called",
  "final_result")

file <- excel_file
create_wkbk(file, df_list, sheetnames)
sheets_index <- c(3, 4, 5)
df_index <- c(4, 4, 5) # normalized sheet gets same format as
# antigen sheet
snprcspf:::format_luminex_results(file, df_list[[4]],
                       sheetnames[3],
                       low_positive_controls_df)

##format_luminex_results(file, df_list[[4]],
##                       sheetnames[4],
##                       low_positive_controls_df)
.df <- df_list[[4]]
sheet_name = sheetnames[3]
wb <- loadWorkbook(excel_file)

cs_positive <- createStyle(fgFill = "red3", wrapText = TRUE)
cs_indeterminate <- createStyle(fgFill = "blue", wrapText = TRUE)
cs_to_repeat <- createStyle(fgFill = "khaki1", wrapText = TRUE) # low positive control wells
cs_header <- createStyle(fgFill = "grey85", wrapText = TRUE)
cs_border_header <- createStyle(fgFill = "grey85", wrapText = TRUE)
positive_row <- matrix(data = rep(1:nrow(.df), each = ncol(.df)),
                       nrow = nrow(.df),
                       ncol = ncol(.df), byrow = TRUE) + 1L
positive_col <- matrix(data = rep(1:ncol(.df), each = nrow(.df)),
                       nrow = nrow(.df),
                       ncol = ncol(.df))

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
  writeData(wb, sheet = sheet_name, x = .df, startRow = positive$row,
            startCol = positive$col, borders = "none",
            borderStyle = "thin",
            headerStyle = cs_positive)
}

if (nrow(positive) > 0) {
  writeData(wb, sheet = sheet_name, x = .df, startRow = indeterminate$row,
            startCol = indeterminate$col, borders = "none",
            borderStyle = "thin",
            headerStyle = cs_indeterminate)
}

if (nrow(positive) > 0) {
  writeData(wb, sheet = sheet_name, x = .df, startRow = to_repeat$row,
            startCol = to_repeat$col, borders = "none",
            borderStyle = "thin",
            headerStyle = cs_to_repeat)
}

writeData(wb, sheet = sheet_name, x = .df, startRow = 1,
          startCol = seq_along(names(.df)), borders = "none",
          borderStyle = "thin",
          headerStyle = cs_header)
writeData(wb, sheet = sheet_name, x = .df, startRow = 1,
          startCol = seq_along(names(.df)), borders = "none",
          borderStyle = "thin",
          headerStyle = cs_border_header)

