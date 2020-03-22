#' Returns samples with "Bn" and "-R" trimmed off of each end
#'
#' @param samples character vector of sample descriptors
#' @import stringi
#' @export
remove_bn_and_dash_R <- function(samples) {
  samples <- stri_trim_both(sapply(stri_trim_both(samples), function(sample) {
    if (!is.na(sample)) {
      if (stri_sub(sample, -2) == "-R")
        sample <- stri_sub(sample, 1, -3)
      if (toupper(stri_sub(sample, 1, 2)) == "BN")
        sample <- stri_sub(sample, 3)
    }
    sample}))
  names(samples) <- NULL
  samples
}
