#' Returns dataframe of possible antigen pairs and agents
#'
#' @export
get_agents_and_antigen_pairs <- function() {
  data.frame(agent = c("STLV", "SRV", "BV", "BV", "SIV", "SIV", "Measles",
                       "T_cruzi"),
             a1 = c("HTLV-1/2", "SRV-2", "BV glyco B", "rBV glycoB",
                    "SIV gp120", "SIV gp130", "rMeasles", "rChagas"),
             a2 = c("STLV p21", "SRV gp-20", "HVP-2", "HVP-2",
                    "SIV mac", "SIV mac", "rMeasles", "rChagas"),
             stringsAsFactors = FALSE)

  # labkey.selectRows(
  #   baseUrl = attr(session, "baseUrl"),
  #   folderPath = "/SNPRC/Core Facilities/SPF Screen Workflow",
  #   schemaName = "lists",
  #   queryName = "agent_antigen_pairs",
  #   viewName = "",
  #   colFilter = NULL,
  #   containerFilter = NULL, colNameOpt = "fieldname"
  # )
}
