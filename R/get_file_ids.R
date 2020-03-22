#' Returns integer vector with plate numbers
#'
#' @param files name of file
#' @import stringi
#' @export
get_file_ids <- function(files) {
  if (all(stri_detect_fixed(tolower(files), "spfcolony"))) {
    file_ids <- unlist(stri_split(files,
                                  fixed = "plate", 2))[seq(2, length(files) * 2,
                                                           by = 2)]
    file_ids <- stri_split(file_ids, regex = "[_, .a-z,A-Z]", 2)
    file_ids <- unlist(file_ids)[seq(1, length(files) * 2, by = 2)]
  } else {
    file_ids <- sapply(stri_extract_all_charclass(files, "[0-9]", merge = TRUE),
                       function(x) {x[[1]]})
  }
  file_ids
}
