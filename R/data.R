#' plate165_mean_raw_mfi_df is a loadable version of the mean_raw_mfi_df dataframe
#' created by `get_mean_raw_mfi_from_xlsx` or `get_mean_raw_mfi_from_lxd` and
#' used by `get_low_positive_controls` and `subtract_background`.
#' @examples
#' \dontrun{
#' plate165_mean_raw_mfi_df <- snprcspf::plate165_mean_raw_mfi_df
#' }
"plate165_mean_raw_mfi_df"
#' plate166_mean_raw_mfi_df is a loadable version of the mean_raw_mfi_df dataframe
#' created by `get_mean_raw_mfi_from_xlsx` or `get_mean_raw_mfi_from_lxd` and
#' used by `get_low_positive_controls` and `subtract_background`.
#' @examples
#' \dontrun{
#' plate166_mean_raw_mfi_df <- snprcspf::plate166_mean_raw_mfi_df
#' }
"plate166_mean_raw_mfi_df"
#' combined_df is a loadable version of the combined_df dataframe
#' created by `get_combined`
#' used by `get_wide_df`, `print_bad_animal_ids` and `print_bad_sample_dates`.
#' @examples
#' \dontrun{
#' plate166_combined_df <- snprcspf::plate166_combined_df
#' }
"plate166_combined_df"
#' plate166_r_mfi_df is a loadable version of the r_mfi_df dataframe
#' created by `r_mfi_df` followed by `basename`
#' used by `basename` (with `basename(r_mfi_df$file)`, `get_combined` and
#' `get_wide_df`.
#' @examples
#' \dontrun{
#' plate166_r_mfi_df <- snprcspf::plate166_r_mfi_df
#' basename(plate166_r_mfi_df$file)
#' }
"plate166_r_mfi_df"
#' plate166_dfs is a loadable version of the dfs list of dataframes
#' created by `get_wide_df`
#' used by `make_excel_wkbk`.
#' @examples
#' \dontrun{
#' dfs <- snprcspf::plate166_dfs
#' low_positive_controls_df <- data.frame()
#' make_excel_wkbk(excel_file, dfs$w_mean_raw_mfi_df, dfs$w_mfi_df,
#'                 dfs$w_d_mfi_df, dfs$w_r_mfi_df, dfs$w_combined_df,
#'                 low_positive_controls_df = low_positive_controls_df)
#' }
"plate166_dfs"
