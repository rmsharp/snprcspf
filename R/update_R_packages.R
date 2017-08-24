#!c:\Program^ Files\R\R-3.4.1\bin\x64\Rscript
### Updates R after new installation of R
##
## This script will use Sys.info() to discover what system it is running on
## and assign the source_path, custom_lib_path, and install_path values.
## However, command line arguments can be used to override these using a
## command such as the following:
## RScript update_R_packages.R "/Users/msharp/Documents/Development/R/r_workspace/library/" "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/"
## This script is written assuming that only base and default packages are
## available.
##
## Function definitions are written using roxygen2 comments so that they could
## be lifted and put into a package as they are. The .Rd files can be created
## using the following commands for the R console with the working directory
## being the location of this file:
## library(roxygen2); roxygenize(package.dir = ".", load_code = roxygen2:::source_package)
##
## Function definitions:
##
#' Get package dependencies
#'
#' Written by Josh O'Brien on stackoverflow on May 13 '15 at 21:42
#' @param path character vector of length one having the directory path to
#' where new version of the custom package exists.
get_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  val[val != "R"]
}
#' Make dependency list
#'
#' Gathers the dependencies from each source package and combines the lists
#' without duplication.
#'
#' @param source_names character vector containing the names of the custom
#' packages to be installed from source.
#' @param path character vector of length one having the path to R's
#' \code{library} directory, which contains the packages being updated from
#' source. This may or may or may not be the same as the system wide
#' \code{library} directory. It could be a user directory.
make_dependency_list <- function(source_names, path) {
  dependencies <- character(0)
  for (name in souce_names) {
    dependencies <- unique(c(dependencies, get_deps(paste0(path, name))))
  }
  dependencies
}
#' Removed these strings
#'
#' Modified from rmsutilityr::remove_strings() by R. Mark Sharp. The
#' modification was to remove a package dependency using the standard
#' relational opporator "==" instead of stri_detect_regex().
#' @param .str character vector that have tokens removed that match
#' tokens within the \code{expunge} vector.
#' @param expunge character vector of tokens to be removed from the
#' \code{.str} vector if present.
#' @param ignore_case boolean that determines whether or not case is ignored.
#' Defaults to FALSE.
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
#' Install R package from package source
#'
#' Takes a list of packages (\code{source_names}) and the path
#' (\code{source_path}) to their common location and
#' installs them into the \code{install_path} directory.
#'
#' @param source_names character vector having one source package name per
#' cell.
#' @param source_path character vector of length one having the directory
#' path of where the package sources (*.tar.gz) reside.
#' @param install_path character vector of length one having the directory
#' path of where packages are to be installed.
install_from_source <- function(source_names, source_path, install_path) {
  for (source_name in source_names) {
    source <- max(list.files(path = source_path,
                             pattern = paste0(source_name, ".*.tar.gz")))
    install.packages(paste0(source_path, source), type = "source", repos = NULL,
                     lib = install_path)
  }
}
## end of function definitions

source_names <- c("rmsutilityr", "animalr", "snprcspf", "snprcpath",
                  "renameSurgerySheets")

node_name <- Sys.info()[["nodename"]]
if (toupper(node_name) == "VGER" | toupper(node_name) == "BOOMER") {
  source_path <- "d:Labkey data/"
  install_path <- "c:/R Library/"
} else {
  install_path <-
    "/Library/Frameworks/R.framework/Resources/library/"
  source_path <- "/Users/msharp/Documents/Development/R/r_workspace/library/"
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  source_path <- args[1]
  install_path <- args[2]
  if (!length(list.files(path = source_path, pattern = ".*tar.gz")) > 0)
    stop(paste0(source_path, " does not contain any package sources. ",
                "I am assuming the path is incorrect"))
  if (!file.exists(paste0(install_path, "utils")))
    stop(paste0(install_path, " does not contain required R package 'utils'. ",
                "I am assuming the path is incorrect."))
}

dependencies <- make_dependency_list(source_names, install_path)

## Remove base packages and local custom packages that are to be installed
## from the vector of dependencies.
remove_these <- c("stats", "tools", "utils", source_names)
dependencies <- remove_these_str(dependencies, expunge = remove_these)

install.packages(dependencies, type = "binary",
                 repos = "https://cran.revolutionanalytics.com",
                 lib = install_path)
update.packages(lib.loc = install_path,
                repos = "https://cran.revolutionanalytics.com", ask = FALSE)

install_from_source(source_names = source_names,
                   source_path = source_path, install_path = install_path)
