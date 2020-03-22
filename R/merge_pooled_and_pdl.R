#' Returns a dataframe with pooled_df merged with pdl_df by pooled_id with
#' pdl_animal_id. All pooled records are retained.
#'
#' @param pooled_df dataframe with pooled samples
#' @param pdl_df dataframe with PDL results.
#' @export
merge_pooled_and_pdl <- function(pooled_df, pdl_df) {
  pooled_df <- merge(pooled_df, pdl_df, by.x = "pool_id",
                     by.y = "pdl_animal_id", all.x = TRUE)
  pooled_df <- pooled_df[!is.na(pooled_df$results), ]
  pooled_df$sample_date <- pooled_df$sample_date.x
  pooled_df$sample_date.x <- NULL
  pooled_df$sample_date.y <- NULL
  pooled_df
}
