#' Returns a dataframe with all bead or all summary results combined
#' depending on which function and column names are passed in.
#'
#' Columns missing in some results set are added with NA values.
#'
#' @param path file path to directory containing files to be read.
#' @param .fun the function object to be used to collect combined results.
#' @param col_names character vector of column names in the order they are to
#' appear.
#' @importFrom openxlsx read.xlsx
#' @import stringi
#' @export
get_all_df <- function(path = "../inst/extdata/", .fun = get_summary_df,
                       col_names = get_col_names("syntactic_summary")) {
  files <- get_usable_files(path = path)
  n_cols <- length(col_names)
  all_df <- data.frame(rbind(1:n_cols))
  names(all_df) <- col_names

  for (file in files) {
    content <- read.xlsx(stri_c(path, "/", file), check.names = FALSE,
                         sep.names = " ")
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    results <- get_result_tables(content)
    part_df <- .fun(results)
    part_df <- data.frame(Filename = rep(file, nrow(part_df)), part_df,
                          stringsAsFactors = FALSE)
    tmp_cols <- names(part_df)
    missing_cols <- col_names[!col_names %in% tmp_cols]
    for (col in missing_cols) {
      part_df <- cbind(part_df, col = rep(NA, nrow(part_df)))
    }
    names(part_df) <- c(tmp_cols, missing_cols)
    part_df <- part_df[ , col_names] # place columns in correct order
    all_df <- rbind(all_df, part_df)
  }
  all_df[-1, ] # remove dummy row
}
