#' Returns the most recent assay file name for each plate number
#'
#' @param files_all character vector of all file names.
#' @export
get_most_recent_files <- function(files_all) {
  if (length(files_all) == 0) {
    return(data.frame(file_id = character(0),
                      file_name = character(0),
                      start_date = character(0),
                      stringsAsFactors = FALSE))
  }
  files_df <- data.frame(file_id = get_file_ids(files_all),
                         file_name = files_all,
                         start_date = get_start_end_dates(files_all)$start_date,
                         stringsAsFactors = FALSE)

  most_recent_files <- data.frame(file_id = character(0),
                                  file_name = character(0),
                                  start_date = as.POSIXct(character()))
  for (file_id in unique(files_df$file_id)) {
    if (nrow(files_df[files_df$file_id == file_id, ]) > 1) {
      most_recent_files <-
        rbind(most_recent_files, get_most_recent_file(file_id, files_df))
    } else {
      most_recent_files <-
        rbind(most_recent_files, files_df[files_df$file_id == file_id, ])
    }
  }
  most_recent_files
}
