#' Returns a dataframe of file names, sample, snprc_id, and sample_date and
#' has the side effect
#' of printing out the bad Ids in a table with file name LocName
#'
#' @param combined_df dataframe with agent assay data results and meta data
#' @param type character string indicating if the output is to be in "latex"
#' format or "html".
#' @param ... extra arguments to \code{xt_print()} that go to \code{print()}
#' @import animalr
#' @import rmsutilityr
#' @export
print_bad_animal_ids <- function(combined_df, type = "latex", ...) {
  controls <- c("ICL CNT High", "ICL CNT Low", "MAC Track High", "MAC Track High",
                "MAC Track Low", "Diluent CNT", "NHP CNT")

  samples <- unique(combined_df$sample[is.na(combined_df$snprc_id) &
                                         !combined_df$sample %in% controls])
  bad_animal_ids_df <- data.frame(file = character(0),
                                  sample = character(0),
                                  snprc_id = character(0),
                                  birth_date = character(0),
                                  colony = character(0),
                                  sample_date = character(0),
                                  stringsAsFactors = FALSE)
  bad_index <- seq_along(combined_df$sample)[combined_df$sample %in% samples]
  if (length(samples) > 0) {
    bad_animal_ids_df <-
      data.frame(
        file = basename(as.character(combined_df$file_name[bad_index])),
        sample = combined_df$sample[bad_index],
        snprc_id = combined_df$snprc_id[bad_index],
        birth_date = combined_df$birth_date[bad_index],
        colony = combined_df$colony[bad_index],
        sample_date = combined_df$sample_date[bad_index],
        stringsAsFactors = FALSE)

    bad_animal_ids_df <- bad_animal_ids_df[!duplicated(bad_animal_ids_df), ]
    if (length(samples) == 1) {
      caption <- stri_c(
        "There was 1 sample identified with an animal Id
        that could not be found in the animal database.")
    } else {
      caption <- stri_c(
        "There were ", length(samples), " samples (", length(unique(samples)), "
      unique samples) identified with animal Ids
        that could not be found in the animal database.")
    }

    xt_print(bad_animal_ids_df, caption = caption,
             label = "tbl:bad-animal-ids", type = type, ...)
  }

  bad_animal_ids_df
}
