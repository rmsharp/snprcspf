#!c:\Program^ Files\R\R-3.4.1\bin\x64\Rscript
### Updates R after new installation of R
##
## This script will use Sys.info() to discover what system it is running on
## and assign the source_path and lib_path values. However, command line
## arguments can be used to override these using a command such as the
## following:
## RScript update_R_packages.R "/Users/msharp/Documents/Development/R/r_workspace/library/" "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/"
#' Get package dependencies
#'
#' Written by Josh O'Brien on stackoverflow on May 13 '15 at 21:42
get_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  val[val != "R"]
}
#' Removed these strings
#'
#' Modified from rmsutilityr::remove_strings() by R. Mark Sharp. The
#' modification was to remove a package dependency using the standard
#' relational opporator "==" instead of stri_detect_regex().
remove_these_str <- function(.str, expunge, ignore_case = FALSE) {
  if (ignore_case) {
    tmp_str <- tolower(.str)
    tmp_expunge <- tolower(expunge)
  }
  else {
    tmp_str <- .str
    tmp_expunge <- expunge
  }
  keep <- rep(TRUE, length(.str))
  for (exp_str in tmp_expunge) {
    keep <- !tmp_str == exp_str & keep
  }
  .str[keep]
}
info <- Sys.info()
if (toupper(info$nodename) == "VGER" | toupper(info$nodename) == "BOOMER") {
  source_path <- "d:Labkey data/"
  lib_path <- "c:/R Library/"
} else {
  lib_path <- "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/"
  source_path <- "/Users/msharp/Documents/Development/R/r_workspace/library/"
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  source_path <- args[1]
  lib_path <- args[2]
  if (!length(list.files(path = source_path, pattern = ".*tar.gz")) > 0)
    stop(paste0(source_path, " does not contain any package sources. ",
                "I am assuming the path is incorrect"))
  if (!file.exists(paste0(lib_path, "utils")))
    stop(paste0(lib_path, " does not contain required R package 'utils'. ",
                "I am assuming the path is incorrect."))
}

## dependencies becomes the collection of packages needed to support our
## custom packages
dependencies <- get_deps(paste0(lib_path, "rmsutilityr"))
dependencies <- unique(c(dependencies, get_deps(paste0(lib_path, "animalr"))))
dependencies <- unique(c(dependencies, get_deps(paste0(lib_path, "snprcspf"))))
dependencies <- unique(c(dependencies,
                              get_deps(paste0(lib_path,
                                              "renameSurgerySheets"))))
## Remove base packages and local custom packages from dependencies
remove_these <- c("stats", "tools", "utils", "rmsutilityr", "animalr",
                  "renameSurgerySheets")
dependencies <- remove_these_str(dependencies, expunge = remove_these)

install.packages(dependencies, type = "binary",
                 repos = "https://cran.revolutionanalytics.com",
                 lib = lib_path)
update.packages(lib.loc = lib_path,
                repos = "https://cran.revolutionanalytics.com", ask = FALSE)

update_from_source <- function(source_names, source_path, lib_path) {
  for (source_name in source_names) {
    source <- max(list.files(path = source_path,
                             pattern = paste0(source_name, ".*.tar.gz")))
    install.packages(paste0(source_path, source), type = "source", repos = NULL,
                     lib = lib_path)
    }
}
source_names <- c("rmsutilityr", "animalr", "renameSurgerySheets")
update_from_source(source_names = source_names,
                   source_path = source_path, lib_path = lib_path)
