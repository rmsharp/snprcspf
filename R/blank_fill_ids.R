#' Returns the Ids in right blank filled and upper case animal Ids.
#'
#' @param ids character vector of animal Ids that may need to be transformed
#' into the proper Id format.
#' @param upper logical indicator of whether ids are to be forced to upper case
#' @examples
#' new_ids <- blank_fill_ids(c("12345", "1X1234", "1234","2 3456"))
#' @import stringi
#' @export
blank_fill_ids <- function(ids, upper = TRUE) {
  if (upper)
    ids <- toupper(stri_trim_both(ids))
  if (class(ids) == "factor")
    ids <- as.character(ids)
  for (i in seq_along(ids)) {
    if (is.na(ids[i]))
      next

    if (stri_length(ids[i]) > 6 | stri_length(ids[i]) < 3)
      warning(stri_c("Id size out of range. i is ", i,
                     " stri_length(ids[[i]]) is ",
                     stri_length(ids[[i]]),
                     " and Id is ", ids[[i]]))
    else
      ids[[i]] <- sprintf("%6s", ids[[i]])
  }
  ids
}
