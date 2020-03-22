#' Returns a dataframe with the Id, Date, and Repeated column added, which
#' are derived from the Animal.ID column.
#'
#' @param df dataframe having at least the Animal.ID column
#' @return a dataframe with the Id, Date, and Repeated column added
#' @export
add_id_date_repeated <- function(df) {
  data.frame(get_id_date_repeated(df$Animal.ID), df, stringsAsFactors = FALSE)
}
