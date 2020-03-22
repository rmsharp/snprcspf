#' Returns species desination of "CYNO", "MACAQUE" or "PAPIO" at this time.
#' If the Id does not belong to a cyno, baboon, or rhesus, the arc_species_code
#' is used.
#'
#' @param conn database connection object
#' @param snprc_id character vector with possible animal Ids
#' @import RODBC
#' @import stringi
#' @export
get_luminex_species <- function(conn, snprc_id) {
  ids <- blank_fill_ids(snprc_id[stri_length(snprc_id) <= 6 &
                                   stri_length(snprc_id) >= 4 &
                                   !is.na(snprc_id)])
  id_str <- vector2string(ids, SS = "', '")
  sql_txt <- stri_c("select cd.id, cd.arc_species_code from current_data cd
     where cd.id in ('", id_str, "')")

  result <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  if (!is.data.frame(result))
    stop(stri_c("get_luminex_species failed with the following error:",
                result))
  if (nrow(result) == 0) {
    stop(stri_c("get_luminex_species failed with an empty dataframe.\n"))
  }
  result$species[result$arc_species_code == "PC"] <- "PAPIO"
  result$species[result$arc_species_code == "MM"] <- "MACAQUE"
  result$species[result$arc_species_code == "MF"] <- "CYNO"
  result$species[!result$arc_species_code %in% c("PC", "MM", "MF")] <-
    result$arc_species_code[!result$arc_species_code %in% c("PC", "MM", "MF")]
  result$id <- blank_fill_ids(result$id)
  id_df <- data.frame(row_order = 1:length(snprc_id), id = snprc_id,
                      stringsAsFactors = FALSE)
  df <- merge(id_df, result, by = "id", all.x = TRUE)
  df$species[order(df$row_order)]
}
