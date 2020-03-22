#' Returns a numeric vector of SqlMed procedure_id given a character
#' vector of luminex_species and luminex_repeatedy.
#'
#'Warning luminex does not provide species designations reliably.
#'This will have to come from post processing.
#'
#' @param luminex_species character vector "MACAQUE" or "PAPIO"
#' @param luminex_repeated characer vector with "Y" or "N" depending on
#' whether the sample ended in "-R" or not.
#' @export
luminex_to_sqlmed_procedure_id <-
  function(luminex_species, luminex_repeated) {
    procedure_id <- integer(length(luminex_species))
    procedure_id[luminex_species == "MACAQUE" |
                   luminex_species == "CYNO"] <- 10637
    procedure_id[luminex_species == "PAPIO"] <- 10636
    procedure_id[luminex_repeated == "Y" & luminex_species == "MACAQUE"] <- 10641
    procedure_id
  }
