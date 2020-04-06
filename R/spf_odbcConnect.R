#' Returns database connection object
#'
#' @param spf_dsn DSN for the SPF user
#' @importFrom RODBC odbcConnect
spf_odbcConnect <- function(spf_dsn) {
  odbcConnect(spf_dsn)
}
