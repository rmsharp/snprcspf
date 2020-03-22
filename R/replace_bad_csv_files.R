#' Returns a dataframe with a row for each bad CSV file with the original
#' name and the renamed file
#'
#' Some of the files from PDL have text trailing the CSV portion of the file.
#' This routine removes files from the end until a record that starts with
#' a token of all integers is found. This is the order primary key.
#' The bad file has been replaces with a cleaned CSV
#' file having the original name.
#' @param path character vector of length one with the file path
#' @param file_names character vector of the file names to be read
#' @param orig_prefix character string with the prefix to be put on the
#' renamed bad file.
#' @import stringi
#' @export
replace_bad_csv_files <- function(path, file_names, orig_prefix = "org") {
  bad_csv_files_df <- data.frame()
  for (file in file_names) {
    replace <- FALSE
    file_txt <- scan(file = stri_c(path, file), what = "character", sep = "\n",
                     quiet = TRUE)
    for (i in rev(seq_along(file_txt)[-1])) {
      #cat(stri_c("Line: ", i, " ", file_txt[i], "\n\n"))
      if (stri_detect_regex(stri_sub(file_txt[i], 2, length = 5),
                            "[[a-zA-Z]]")) {
        last <- i - 1
        replace <- TRUE
      } else {
        break
      }
    }
    if (replace) {
      file.rename(from = stri_c(path, file), to = stri_c(path,
                                                         orig_prefix, file))
      write(file_txt[1:last], file = stri_c(path, file))
      bad_csv_files_df <-
        rbind(bad_csv_files_df,
              data.frame(original_name = stri_c(path, file),
                         renamed_file = stri_c(path, orig_prefix, file)))
    }
  }
  bad_csv_files_df
}
