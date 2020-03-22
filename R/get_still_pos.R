#' Get dataframe of animals where the most recent assay was not negative.
#'
#' A PCR assay trumps an non-PCR assay so that if a PCR assay is negative and
#' and non-PCR assay is non-negative, the animal is negative for that agent.
#'
#' @return A dataframe with assays whose last value are non-negative.
#'
#' @param sorted_df dataframe sorted values of all assays.
#' @import stringi
#' @import sqldf
#' @export
get_still_pos <- function(sorted_df) { # renamed still_pos()
  assay_pairs_df <- get_assay_pairs()
  still_pos_df <- data.frame()
  for (i in seq_along(assay_pairs_df$agent)) {
    ab_assay <- assay_pairs_df$ab_assay[i]
    pcr_assay <- assay_pairs_df$pcr_assay[i]
    last_assay_is_nonnegative <-
      sqldf(stri_c(
        "select max(n_neg_df.sample_date),
        n_neg_df.* from n_neg_df
        where not exists(select 1 from sorted_df
        where sorted_df.sample_date > n_neg_df.sample_date
        and n_neg_df.id = sorted_df.id
        and sorted_df.`", ab_assay, "` != 'N')"))
    results <- sqldf(stri_c(
      "select * from last_assay_is_nonnegative a
      where not exists(select 1 from sorted_df where sorted_df.`",
      pcr_assay, "` = 'N' and sorted_df.id = a.id)"))
    still_pos_df <-
      rbind(still_pos_df,
            data.frame(agent = rep(assay_pairs_df$agent[i], nrow(results)),
                       results, check.names = FALSE))
  }
  still_pos_df
}
