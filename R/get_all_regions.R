#' Returns a dataframe with the files, order, and Region[@@name]s
#' for a specific Setup, for example 'DefaultSetup'.
#'
#' @param files character vector of files to be processed
#' @param setup_name name of setup in XML file to get regions from
#' @export
get_all_regions <- function(files, setup_name = "DefaultSetup") {
  regions_df <- data.frame(file = character(0), order = integer(0),
                           name = character(0), id = character(0),
                           stringsAsFactors = FALSE)
  for (file in files) {
    regions_df <-
      rbind(regions_df, data.frame(file = basename(file),
                                   get_region_df(file, setup_name),
                                   stringsAsFactors = FALSE))
  }
  regions_df
}
