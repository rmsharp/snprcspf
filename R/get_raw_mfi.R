#' Returns dataframe of original values
#'
#' @param conn database connection object
#' @param file character vector with filename
#' @import XML
#' @export
get_raw_mfi <- function(conn, file) {
  lum <- xmlTreeParse(file, getDTD = FALSE, useInternalNodes = TRUE)
  nodes <- getNodeSet(lum, "/WorkSet//Plate//Well//LocName")
  well_names <- xmlSApply(nodes, xmlValue)
  animal_ids <- get_id_from_sample(conn, well_names)
  sample_dates <- get_date_from_sample(conn, well_names)
  report_date <- stri_split_fixed(get_session_date_tm(lum), " ")[[1]][[1]]
  report_dates <- rep(report_date, length(well_names))
  n_wells <- length(well_names)
  cat(stri_c("There are ", n_wells, " wells.\n"))
  # there is just one plate but it needs to be unwrapped.
  plate <- getNodeSet(lum, "/WorkSet//Plate")[[1]]
  region_df <- get_region_df(file)
  well_rows <- LETTERS[as.numeric(
    unlist(getNodeSet(plate, "//Well/@row"))) + 1]
  well_cols <- as.character(as.numeric(
    unlist(getNodeSet(plate, "//Well/@col"))) + 1)
  ids <- as.character(
    unlist(getNodeSet(plate, "//Well//RSts[@id != '0']/@id")))
  raw_mfi <- data.frame(
    file = rep(basename(file), length(ids)),
    sample = rep(well_names, 1, each = nrow(region_df)),
    well_row = rep(well_rows, 1, each = nrow(region_df)),
    well_col = rep(well_cols, 1, each = nrow(region_df)),
    id = ids,
    animal_id = rep(animal_ids, 1, each = nrow(region_df)),
    sample_date = rep(sample_dates, 1, each = nrow(region_df)),
    report_date = rep(report_dates, 1, each = nrow(region_df)),
    name = rep(region_df$name, length(ids) / nrow(region_df)),
    val = as.numeric(unlist(getNodeSet(plate,
                                       "//Well//RSts[@id != '0']/@val"))),
    cv = as.numeric(unlist(getNodeSet(plate,
                                      "//Well//RSts[@id != '0']/@cv"))),
    stringsAsFactors = FALSE
  )
  raw_mfi$sd <- raw_mfi$val / raw_mfi$cv
  raw_mfi
}
