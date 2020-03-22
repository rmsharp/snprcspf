#' Returns an integer vector with the the positions in the line that begin
#' each column label.
#'
#' @param content charcter vector containing results. First line should contain
#' column labels.
#' @import stringi
get_col_boundaries <- function(content) {
  line <- content[1]
  start_end <- stri_locate_all_words(line)[[1]]
  start <- start_end[ , 1][c(1, 2, 4, 5, 6, 7, 8, 10, 11)]
  # Header is shifted one character to left of data for Animal Id
  start[2] <- start[2] - 1
  # Header is shifted one character to left of data for results
  start[9] <- start[9] - 1
  end <- as.integer(c(start[2:length(start)] - 1, max(nchar(content))))
  col_boundaries <- data.frame(sample = c(start[1], end[1]),
                               pdl_animal_id = c(start[2], end[2]),
                               pdl_species = c(start[3], end[3]),
                               sample_type = c(start[4], end[4]),
                               sample_date = c(start[5], end[5]),
                               comment = c(start[6], end[6]),
                               test_type = c(start[7], end[7]),
                               results = c(start[8], end[8]),
                               test_comment = c(start[9], end[9]))
  col_boundaries
}
