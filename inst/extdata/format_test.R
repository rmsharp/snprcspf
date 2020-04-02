library(snprcspf)
library(stringi)
library(rmsutilityr)
library(lubridate)

prefix <- stri_c(stri_replace_all_fixed(stri_replace_all_fixed(
  now(), " ", "_"), ":", "-"), "_")
input_file <- "2020-03-21_12-45-43_SPF_Colony_Testing_Plate_166_03-06-20.xlsx"
file_no_ext <- stri_sub(basename(input_file), 1,
                        stri_locate_last_fixed(basename(input_file),
                                               pattern = ".")[[1]] - 1)
excel_file <- stri_c(prefix, file_no_ext, ".xlsx")
dfs <- snprcspf::plate166_dfs
dfs$w_mean_raw_mfi_df$file <- "SPF_Colony_Testing_Plate_166_03-06-20.xlsx"
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
for (i in seq_along(sheets_index)) {
  df_index <- c(4, 4, 5) # normalized sheet gets same format as
  # antigen sheet
  snprcspf:::fmt_luminex_results(file, df_list[[df_index[i]]],
                         sheetnames[sheets_index[i]],
                         low_positive_controls_df)
}
file

##format_luminex_results(file, df_list[[4]],
##                       sheetnames[4],
##                       low_positive_controls_df)
