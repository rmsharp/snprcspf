#' Returns Machine serialNumber, name, sftwareVersion, firmwareVersion, and
#' DSPVersion
#'
#' @param file name of file containing XML
#' @import XML
#' @export
get_Machine_info <- function(file) {
  lum <- xmlTreeParse(file, getDTD = FALSE, useInternalNodes = TRUE)
  info <- sapply(getNodeSet(lum, "//MachineInfo"), xmlAttrs)
  serialNumber <- na_to_char(sapply(info, function(x) {
    x["serialNumber"]}), "NULL")
  name <- na_to_char(sapply(info, function(x) {x["name"]}), "NULL")
  softwareVersion <- na_to_char(sapply(info, function(x) {
    x["softwareVersion"]}), "NULL")
  firmwareVersion <- na_to_char(sapply(info, function(x) {
    x["firmwareVersion"]}), "NULL")
  DSPVersion <- na_to_char(sapply(info, function(x) {
    x["DSPVersion"]}), "NULL")

  data.frame(file = basename(file),
             serialNumber = serialNumber,
             name = name,
             softwareVersion = softwareVersion,
             firmwareVersion = firmwareVersion,
             DSPVersion = DSPVersion,
             stringsAsFactors = FALSE)
}
