#' Returns the Setup[@@name]s for a lum
#'
#' @param lum XML object
#' @param setup indicates which Setup to return the name for. 0 indicates
#' return all setup names.

get_setup_name <- function(lum, setup = 0) {
  nodes <- getNodeSet(lum, stri_c("//Setup[@name]"))
  if (setup == 0) {
    setup_names <- character(length(nodes))
    for (i in seq_along(nodes)) {
      setup_names[i] <- sapply(nodes, xmlAttrs)[[i]]["name"]
    }
  } else {
    setup_names <- sapply(nodes, xmlAttrs)[[setup]]["name"]
  }
  setup_names
}
