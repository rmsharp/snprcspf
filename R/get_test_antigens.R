#' Returns the list of test antigens
#'
#' Uses \code{get_agents_and_antigen_pairs()} to create the list of unique
#' test antigens
#'
#' @export
get_test_antigens <- function() {
  antigen_pairs <- get_agents_and_antigen_pairs()
  antigens <- c()
  for (i in 1:nrow(antigen_pairs)) {
    antigens <- c(antigens, antigen_pairs$a1[i], antigen_pairs$a2[i])
  }
  unique(antigens)
}
