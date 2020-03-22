#' Returns a character vector of SqlMed test_name given a character
#' vector of luminex_agent.
#'
#' @param luminex_agent character vector of pdl_test; case sensitive
#' @export
luminex_agent_to_sqlmed_test_name <- function(luminex_agent) {
  as.character(
    c(
      "SIV" = "SIV AB",
      "SRV" = "SRV AB",
      "BV" = "HERPES B VIRUS",
      "STLV" = "STLV-1 AB",
      "Measles" = "MEASLES",
      "T_cruzi" = "T. CRUZI AB")[luminex_agent])
}
