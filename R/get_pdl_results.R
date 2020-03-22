#' Get PDL results
#'
#' @return Dataframe with all contents of the pdl_results table except the
#' timestamp column.
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
get_pdl_results <- function(conn) {
  pdl_results_df <- sqlQuery(conn, "select * from pdl_results",
                             stringsAsFactors = FALSE)
  pdl_results_df$TIMESTAMP <- NULL
}
