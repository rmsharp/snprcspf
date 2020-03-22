#' Returns pdl data from their CSV files given a list of filenames and the
#' relative or absolute path.
#'
#' @param file_names character vector with the files to be read
#' @param path character string with the path
#' @import stringi
#' @export
get_pdl_csv_data <- function(file_names, path) {
  pdl_df <- data.frame()
  for (file in as.character(file_names)) {
    file_df <- utils::read.csv(stri_c(path, file), stringsAsFactors = FALSE,
                               check.names = FALSE, fill = TRUE, na.strings = "N/A")
    # cat(stri_c("Read: ", file, ", ", vector2string(names(file_df),
    #                                                SS = "', '"), "\n\n"))
    file_df <- file_df[ , 1:24]
    pdl_df <-
      rbind(pdl_df, data.frame(file = rep(basename(file), nrow(file_df)),
                               print_date_tm = rep("", nrow(file_df)), file_df,
                               stringsAsFactors = FALSE, check.names = FALSE))
  }
  pdl_df <- fix_pdl_csv_columns(pdl_df)

  pdl_df <-
    pdl_df[!stri_detect_fixed(pdl_df$test_type, pattern = "ABSCN-5"), ]
  pdl_df[is.na(pdl_df)] <- ""
  pdl_df
}
