library(lintr)
mylinters <- default_linters
mylinters$camel_case_linter <- NULL
mylinters$commented_code_linter <- NULL
mylinters$spaces_inside_linter <- NULL
#mylinters$camel_case_linter <- NULL

#mylinters$camel_case_linter <- NULL
lint(filename = "snprcspf.R", linters = mylinters)
