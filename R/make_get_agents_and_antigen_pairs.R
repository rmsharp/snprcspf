#' #' Makes function to return agents and antigen pairs.
#' #'
#' #' @param session session object from LabKey.
#' #' @import Rlabkey
#' make_get_agents_and_antigen_pairs <- function(session) {
#'   schema_conn <- getSchema(session, "lists")
#'   schema_element <- schema_conn[["agent_antigen_pairs"]]
#'   agents_and_antigen_pairs <- getRows(session, query)
#'   f <- function() {
#'     agents_and_antigen_pairs
#'   }
#' }
#'
