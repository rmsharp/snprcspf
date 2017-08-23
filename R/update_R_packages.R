## Updates R after new installation of R
get_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names=FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  val[val != "R"]
}
remove_these_str <- function (.str, expunge, ignore_case = FALSE)
{
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

lib_path <- "c:/R Library/"
lib_path <- "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/"
source_path <- "/Users/msharp/Documents/Development/R/r_workspace/library/"

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
