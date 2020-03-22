#' Returns /WorkSet/Description from XML
#'
#' @param lum XML object
#' @import XML
#' @export
get_workset_desc <- function(lum) {
  node <- getNodeSet(lum, "/WorkSet/Description")
  xmlValue(node[[1]])
}
