#' Returns df dataframe with the SqlMed procedure_name, procedure_id,
#' test_id, and test_name values added to the df.
#'
#' @param df dataframe with the data from the PDL reports.
#' @param lab character vector of length one that indicates the laboratory
#' @export
add_sqlmed_codes <- function(df, lab) {

  if (toupper(lab) == "PDL") {
    df$procedure_id <- pdl_assay_to_sqlmed_procedure_id(df$test_type)
    df$procedure_name <- pdl_assay_to_sqlmed_procedure_name(
      df$test_type)
    df$test_id <- pdl_assay_to_sqlmed_test_id(df$test_type)
    df$test_name <- pdl_assay_to_sqlmed_test_name(df$test_type)
  } else if (toupper(lab) == "GIAVEDONI") {
    df$procedure_id <- luminex_to_sqlmed_procedure_id(df$species, df$repeated)
    df$procedure_name <- luminex_to_sqlmed_procedure_name(df$species,
                                                          df$repeated)
    df$test_id <- luminex_agent_to_sqlmed_test_id(df$agent)
    df$test_name <- luminex_agent_to_sqlmed_test_name(df$agent)
  }
  df
}
