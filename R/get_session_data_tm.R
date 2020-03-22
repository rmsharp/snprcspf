#' Returns date and time of assay based on '//Plate[@@name].
#'
#' @param lum XML object
#' @import stringi
#' @import XML
#' @export
get_session_date_tm <- function(lum) {
  node <- getNodeSet(lum, "//Plate")
  if (length(node) > 1)
    stop(stri_c("More than one plate found in a file.", length(node),
                "were found."))
  date_tm <- stri_sub(sapply(node, xmlAttrs)["name", 1], 8)
  stri_c(stri_sub(date_tm, 1, 4), "-", stri_sub(date_tm, 5, 6), "-",
         stri_sub(date_tm, 7, 8), " ", stri_sub(date_tm, 9, 10), ":",
         stri_sub(date_tm, 11, 12), ":", stri_sub(date_tm, 13, 14))
}
