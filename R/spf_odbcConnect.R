#' Returns database connection object
#'
#' @param spf_dsn DSN for the SPF user
#' @import RODBC
spf_odbcConnect <- function(spf_dsn) {
  odbcConnect(spf_dsn)
}
