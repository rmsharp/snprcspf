#' Returns a dataframe with the agent and two antigens used to assay for it.
#'
#' Since only one antigen is used for Measles and T_cruzi, they are repeated.
#'
#' Charles River changed one of their beads without any warning.
#' Specifically, they dropped SIV gp130 (region 34) and replaced it for
#' SIV gp120 (region 63).
#' @param antigens vector of antigens
#'
#' @export
get_antigen_pairs <- function(antigens) {
  antigens <- unique(antigens)
  antigen_pairs <- get_agents_and_antigen_pairs()
  tmp_pairs <- antigen_pairs[antigen_pairs$a1 %in% antigens, ]
  remaining_ag <- antigens[!antigens %in% tmp_pairs$a1]
  remaining_ag <- remaining_ag[!remaining_ag %in% tmp_pairs$a2]

  tmp2_pairs <- antigen_pairs[antigen_pairs$a2 %in% remaining_ag, ]
  tmp2_pairs <- tmp2_pairs[!duplicated(tmp2_pairs$a2), ]
  tmp3_pairs <- rbind(tmp_pairs, tmp2_pairs)
  data.frame(tmp3_pairs[!duplicated(tmp3_pairs), ], stringsAsFactors = FALSE,
             row.names = NULL)
}
