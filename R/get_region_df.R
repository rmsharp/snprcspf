#' Returns a dataframe with region order, region name, and region id
#'
#' @param file name of file
#' @param setup_name value of name attribute of <Setup>
#' @import XML
#' @export
get_region_df <- function(file, setup_name = "DefaultSetup") {
  lum <- xmlTreeParse(file, getDTD = FALSE, useInternalNodes = TRUE)
  nodes <- getNodeSet(lum, stri_c(
    "//Setup[@name='", setup_name, "']/Region[@name]"))
  n <- length(nodes)
  regions <- sapply(nodes, xmlAttrs)
  region_names <- regions["name", ]
  region_ids <- regions["id", ]

  data.frame(order = (1:n), name = region_names, id = region_ids,
             stringsAsFactors = FALSE)
}
