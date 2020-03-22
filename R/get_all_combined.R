#' Returns dataframe with multiple combined results from antigen results
#'
#' The following results set are possible:
#'  antigen 1 P and antigen 2 P, combined is P
#'  antigen 1 P and antigen 2 N, combined is I
#'  antigen 1 N and antigen 2 P, combined in I
#'  antigen 1 N and antigen 2 N, combined in N
#'
#' @param conn database connection object
#' @param all_r_mfi_df antigen results
#' @import stringi
#' @export
get_all_combined <- function(conn, all_r_mfi_df) {
  all_combined_df <- data.frame()
  for (file in as.character(unique(all_r_mfi_df$file))) {
    r_mfi_df <- all_r_mfi_df[all_r_mfi_df$file == file, ]
    combined_df <- get_combined(conn, r_mfi_df, file)
    all_combined_df <- rbind(all_combined_df, combined_df)
  }
  all_combined_df
}
