#' Returns data frame with file and start_date as the two columns give
#' a list of luminex data file names
#'
#' Values are taken from '//Plate[@@startDate]"
#' @param files character vector with file names (with relative or absolute
#' path).
get_start_end_dates <- function(files) {
  start_dates <- sapply(files, function(file) {
    xmlAttrs(
      xmlChildren(xmlRoot(xmlTreeParse(file)))$Plate)[["startDate"]]
  })
  end_dates <- sapply(files, function(file) {
    xmlAttrs(
      xmlChildren(xmlRoot(xmlTreeParse(file)))$Plate)[["endDate"]]
  })
  data.frame(file = names(start_dates),
             start_date = suppressWarnings(strptime(as.character(start_dates),
                                                    format = "%Y-%m-%dT%H:%M:%S")),
             end_date = suppressWarnings(strptime(as.character(end_dates),
                                                  format = "%Y-%m-%dT%H:%M:%S")),
             stringsAsFactors = FALSE)
}
