#' Returns the most recent assay file name for each plate number
#' @param file_id integer value of plate number
#' @param files_df dataframe with file_id, file name and file date.
#' @import stringi
#' @export
get_most_recent_file <- function(file_id, files_df) {
  files_subset <- files_df[files_df$file_id == file_id, ]
  files_subset <- files_subset[files_subset$start_date ==
                                 max(files_subset$start_date), ]
  if (nrow(files_subset) > 1) {
    files_subset <-
      files_subset[stri_detect(files_subset$file_name,
                               fixed = "reclassified"), ]
  }
  files_subset
}
