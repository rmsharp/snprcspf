#' Returns a logical value indicating whether or not the list of divisor
#' names in
#' the file ``cutoff.xlsx'' is the same as the list within the .lxd file.
#'
#' @param file character vector with the name of the name, including path,
#'  of the XML raw datafile
#' @import stringi
#' @export
divisors_match <- function(file) {
  divisor_values <- get_divisors(basename(as.character(file)))
  divisor_cat <- stri_c("after divisors_values ", file, " length = ",
                        length(divisor_values), "\n")
  region_df <- get_region_df(file, setup_name = "DefaultSetup")
  region_cat <- stri_c("after get_region_df ", file, " length = ",
                       nrow(region_df), "\n\n")
  result <- all(sort(names(divisor_values)) == sort(region_df$name))
  if (!result) {
    cat(divisor_cat)
    cat(region_cat)
  }
  result
}
