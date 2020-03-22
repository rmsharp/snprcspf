#' Returns a dataframe of file names, LocName, animal Id, and sample date and
#' has the side effect
#' of printing out the bad sample dates in a table with file name LocName
#'
#' @param conn database connection object
#' @param combined_df dataframe with agent assay data results and meta data
#' @param type character string indicating if the output is to be in "latex"
#' format or "html".
#' @param ... extra arguments to \code{xt_print()} that go to \code{print()}
#' @import animalr
#' @import rmsutilityr
#' @export
print_bad_sample_dates <- function(conn, combined_df, type = "latex", ...) {
  id_df <- data.frame(id = combined_df$snprc_id,
                      sample_date = combined_df$sample_date,
                      stringsAsFactors = FALSE)
  # If you do not have an Id or a date we cannot check.
  keep <- !(is.na(id_df$id) | is.na(id_df$sample_date))
  id_df <- id_df[keep, ]
  combined_df <- combined_df[keep, ]
  #cat('file = ', file, "\n")
  is_id <- is_animal_alive(conn, id_df)
  bad_sample_dates_df <-
    data.frame(file = character(0),
               sample = character(0),
               snprc_id = character(0),
               birth_date = character(0),
               colony = character(0),
               sample_date = character(0),
               stringsAsFactors = FALSE)
  if (any(!is_id)) {
    bad_sample_dates_df <-
      data.frame(file = basename(as.character(combined_df$file_name[!is_id])),
                 sample = combined_df$sample[!is_id],
                 snprc_id = combined_df$snprc_id[!is_id],
                 birth_date = combined_df$birth_date[!is_id],
                 colony = combined_df$colony[!is_id],
                 sample_date = combined_df$sample_date[!is_id],
                 stringsAsFactors = FALSE)
    bad_sample_dates_df <-
      bad_sample_dates_df[!duplicated(bad_sample_dates_df), ]
    if (nrow(bad_sample_dates_df) == 1) {
      caption <- stri_c(
        "There was 1 bad sample date identified where the animal was not
        present on the date indicated.")
    } else {
      caption <- stri_c(
        "There were ", nrow(bad_sample_dates_df), " bad sample dates identified
        where the animals were not present on the dates indicated.")
    }


    xt_print(bad_sample_dates_df, caption = caption,
             label = "tbl:bad-sample-dates", type = type, ...)
  }
  bad_sample_dates_df
}
