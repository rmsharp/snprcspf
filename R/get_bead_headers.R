#' Returns a dataframe with all headers found in the bead results as seen by
#' get_bead_df()
#'
#' @param path file path to directory containing files to be read.
#' @param files names of files to be read
#' @importFrom openxlsx read.xlsx
#' @import stringi
#' @export
get_bead_headers <- function(path, files) {
  max_len <- 0
  headers <- list(length(files))

  for (i in seq_along(files)) {
    content <- read.xlsx(stri_c(path, "/", files[i]), check.names = FALSE,
                         sep.names = " ")
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    results <- get_result_tables(content)
    bead_df <- get_bead_df(results = results)
    len <- length(bead_df)
    headers[i] <- list(c(files[i], names(bead_df)))
    if (len > max_len)
      max_len <- len
  }
  max_len <- max_len + 1 # room for filename
  new_headers <- list(length(files))
  for (i in seq_along(headers)) {
    header <- headers[[i]]
    header <- c(header, rep(NA, max_len - length(header)))
    new_headers[i] <- list(header)
  }
  headers <- new_headers
  header_df <- rbind(1:max_len)
  names(header_df) <- stri_c("col_", 1:max_len)
  for (i in seq_along(headers)) {
    header_df <- rbind(header_df, headers[[i]])
  }
  header_df <- data.frame(header_df, stringsAsFactors = FALSE)
  names(header_df) <- c("Filename", stri_c("Col_", 1:(max_len - 1)))
  header_df[-1, ]
}
