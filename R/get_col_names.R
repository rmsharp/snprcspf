#' Returns character vector of column names base on type requested.
#'
#' type can be one of c("bead", "syntactic_bead", "summary",
#' "syntactic_summary")
#' @param type character vector of length one having a column type
#' @import rmsutilityr
#' @import stringi
#' @export
get_col_names <- function(type) {
  valid_cols <- c("raw", "syntactic_raw", "bead", "syntactic_bead", "summary",
                  "syntactic_summary", "agent")
  if (!any(type %in% valid_cols)) {
    stop(stri_c(type, " is not a valid column type name. Must be one of ",
                get_and_or_list(valid_cols), "."))
  }
  control_cols <- c("Human IgG", "wBAC", "Goat anti-human IgG")
  antigen_cols <- get_test_antigens()
  raw_cols <- c(antigen_cols, control_cols)
  bead_cols <- c("Filename", "Animal ID", antigen_cols)
  agent_cols <- unique(get_agents_and_antigen_pairs()$agent)
  summary_cols <- c("Filename", "Animal.ID", agent_cols)

  col_names_list <- list(
    raw = raw_cols,
    sytactic_raw = make.names(raw_cols),
    bead = bead_cols,
    syntactic_bead = make.names(bead_cols),
    summary = summary_cols,
    syntactic_summary = make.names(summary_cols),
    agent = agent_cols,
    syntactic_agent = make.names(agent_cols))
  col_names_list[type][[1]]
}
