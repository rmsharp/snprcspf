#' Returns pooled_df dataframe with bleed_date replaced by sample_date.
#'
#' @param pooled_df dataframe with bleed_date in yyyymmdd format.
#' @param sep character vector of length one having the character(s) to be used
#' @return \code{pooled_df} dataframe with \code{sample_date} in yyyy-mm-dd
#' format.
#' @import stringi
#' @export
pooled_bleed_date_to_sample_date <- function(pooled_df, sep = "-") {
  pooled_df$sample_date <-
    stri_datetime_format(
      stri_datetime_parse(pooled_df$bleed_date, format = "uuuuMMdd"),
      format = "uuuu-MM-dd")
  # pooled_df$sample_date <- stri_c(stri_sub(pooled_df$bleed_date, 1, 4),
  #                                 rep(sep, nrow(pooled_df)),
  #                                 stri_sub(pooled_df$bleed_date, 5, 6),
  #                                 rep(sep, nrow(pooled_df)),
  #                                 stri_sub(pooled_df$bleed_date, 7, 8)
  # )
  pooled_df$bleed_date <- NULL
  pooled_df
}
