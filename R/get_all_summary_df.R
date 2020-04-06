#' Returns a dataframe with all summary results combined.
#'
#' Columns missing in some results set are added with NA values.
#'
#' @param path file path to directory containing files to be read.
#' @param col_names character vector of column names in the order they are to
#' appear.
#' @importFrom openxlsx read.xlsx
#' @import stringi
#' @export
get_all_summary_df <- function(path = "../inst/extdata/",
                               col_names = c("Filename", "Animal.ID", "STLV",
                                             "SRV", "BV", "SIV", "Measles")) {
  files <- get_usable_files(path = path)
  n_cols <- length(col_names)
  all_summary_df <- data.frame(rbind(1:n_cols))
  names(all_summary_df) <- col_names

  for (file in files) {
    content <- read.xlsx(stri_c(path, "/", file), check.names = FALSE,
                         sep.names = " ")
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    results <- get_result_tables(content)
    summary_df <- get_summary_df(results)
    summary_df <- data.frame(Filename = rep(file, nrow(summary_df)), summary_df,
                             stringsAsFactors = FALSE)
    tmp_cols <- names(summary_df)
    missing_cols <- col_names[!col_names %in% tmp_cols]
    for (col in missing_cols) {
      summary_df <- cbind(summary_df, col = rep(NA, nrow(summary_df)))
    }
    names(summary_df) <- c(tmp_cols, missing_cols)
    summary_df <- summary_df[ , col_names] # place columns in correct order
    all_summary_df <- rbind(all_summary_df, summary_df)
  }
  all_summary_df[-1, ] # remove dummy row
}
