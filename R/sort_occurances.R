#' Returns a sorted dataframe with First occurrances appearing in
#' chronological order followed by all subsequent assay results.

#' Sample results based on pooled samples are included. If an animal is lost
#' to followup due to leaving the institution or death a record indicating that
#' event and date is the last record in the group.
#'
#' @param followup_df dataframe with all records.
#' @return sorted \code{followup_df} as \code{sorted_df}
#' @importFrom stringi stri_detect_fixed
#' @export
sort_occurances <- function(followup_df) {
  f_df <- followup_df[stri_detect_fixed(followup_df$flag, "FIRST"), ]
  f_df <- f_df[order(f_df$sample_date, f_df$report_date), ]
  c_df <- followup_df[stri_detect_fixed(followup_df$flag, "Confirm"), ]
  c_df <- c_df[order(c_df$sample_date, c_df$report_date), ]
  e_df <- followup_df[stri_detect_fixed(followup_df$flag, "Exit"), ]
  e_df <- e_df[order(e_df$sample_date, e_df$report_date), ]
  p_df <- followup_df[stri_detect_fixed(followup_df$flag, "Pooled"), ]
  p_df <- p_df[order(p_df$sample_date, p_df$report_date), ]

  sorted_df <- data.frame()
  for (i in seq_along(f_df$flag)) {
    sorted_df <- rbind(sorted_df, f_df[i, ])
    tmp_df <- c_df[c_df$id == f_df$id[i], ]
    for (j in seq_along(tmp_df$flag)) {
      sorted_df <- rbind(sorted_df, tmp_df[j, ])
    }
    tmp_df <- p_df[p_df$id == f_df$id[i], ]
    for (j in seq_along(tmp_df$flag)) {
      sorted_df <- rbind(sorted_df, tmp_df[j, ])
    }
    sorted_df <- rbind(sorted_df, e_df[e_df$id == f_df$id[i], ])
  }
  drop_cols <- character(0)
  for (col in names(sorted_df)) {
    if (all(is.na(sorted_df[ , col])))
      drop_cols <- c(drop_cols, col)
  }
  sorted_df <- sorted_df[, !(names(sorted_df) %in% drop_cols)]
  sorted_df
}
