#' Returns Plate[@@plateID] (plateID attribute of the Plate node.)
#'
#' @param lum XML object
#' @importFrom stringi stri_c
#' @importFrom XML getNodeSet xmlAttrs
#' @export
get_plateID <- function(lum) {
  node <- getNodeSet(lum, "//Plate")
  if (length(node) > 1)
    stop(stri_c("More than one plate found in a file.", length(node),
                "were found."))
  as.character(sapply(node, xmlAttrs)[1])
}
