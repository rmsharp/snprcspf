#' Returns a named numeric vector with the divisors originally provided by the
#' vendor and copied into the Excel sheet "../inst/extdata/cutoff.xlsx"
#'
#' @param file full or relative path name of the Luminex file being analyzed.
#' @import readxl
#' @import stringi
#' @export
get_divisors <- function(file) {
  if (stri_detect_fixed(file, "spfcolony")) {
    divisors_df <- readxl::read_excel("../inst/extdata/cutoff.xlsx")
    divisors_df <- divisors_df[!is.na(divisors_df$file), ]
    divisors_df <- divisors_df[stri_detect_fixed(divisors_df$file,
                                                 pattern = basename(file)), ]
    divisor_names <- as.character(divisors_df[1, 4:ncol(divisors_df)])
    divisor_values <- as.numeric(divisors_df[2, 4:ncol(divisors_df)])
    divisor_names <- divisor_names[!is.na(divisor_names)]


    divisor_values <- divisor_values[!is.na(divisor_values)]
    names(divisor_values) <- divisor_names
  } else if (any(stri_detect_regex(toupper(excel_sheets(file)), "MAC TRACK"))) {
    ## Has a MAC TRACK worksheet added
    sheet_names <- excel_sheets(file)
    mac_sheet <- sheet_names[stri_detect_regex(toupper(sheet_names),
                                               "MAC TRACK")]
    content <- read_excel(file, sheet = mac_sheet)
    divisor_values <- get_excel_divisors(content)
  } else {
    content <- readxl::read_excel(file)
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    results <- get_result_tables(content)
    divisor_values <- get_excel_divisors(results)
  }
  divisor_values
}
