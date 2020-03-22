#' Returns a numeric vector of SqlMed test_id given a character
#' vector of luminex_agent.
#'
#' @param luminex_agent character vector of pdl_test; case sensitive
#' @export
luminex_agent_to_sqlmed_test_id <- function(luminex_agent) {
  as.integer(
    c("Measles" = 786,
      "SIV" = 788,
      "SRV" = 787,
      "BV" = 785,
      "STLV" = 789,
      "T_cruzi" = 850)[luminex_agent])
}
