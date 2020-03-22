#' Returns the mean_raw_mfi_df from an XML file (.lxd extension) from the
#' Luminex machine.
#'
#' @param conn database connection object
#' @param file fully qualified file name of XML file
#' @export
get_mean_raw_mfi_from_lxd <- function(conn, file) {
  raw_mfi_df <- get_raw_mfi(conn, file) # has possible repeats
  get_mean_raw_mfi(raw_mfi_df) # returns mean of repeats
}
