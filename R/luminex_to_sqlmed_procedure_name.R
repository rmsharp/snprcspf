#' Returns a character vector of SqlMed procedure_names given a character
#' vector of Luminex_assay.
#'
#' @param luminex_species character vector of agent; case sensitive
#' @param luminex_repeated character vector of whether the assay was
#' repeated for the animal
#' @export
luminex_to_sqlmed_procedure_name <-
  function(luminex_species, luminex_repeated) {
    procedure_name <- character(length(luminex_species))
    procedure_name[luminex_species == "MACAQUE" |
                     luminex_species == "CYNO"] <- "RHESUS SPF SCREEN"
    procedure_name[luminex_species == "PAPIO"] <- "BABOON SPF SCREEN"
    procedure_name
  }
