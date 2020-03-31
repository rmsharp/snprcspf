#' Returns dataframe with combined results from antigen results
#'
#' The following results set are possible:
#'  antigen 1 P and antigen 2 P, combined is P
#'  antigen 1 P and antigen 2 N, combined is I
#'  antigen 1 N and antigen 2 P, combined in I
#'  antigen 1 N and antigen 2 N, combined in N
#'
#' @param conn database connection object
#' @param r_mfi_df antigen results
#' @param file character vector with name of file
#' @import stringi
#' @export
get_combined <- function(conn, r_mfi_df, file) {
  df <- data.frame(row_order = 1:nrow(r_mfi_df), r_mfi_df)
  ag_pair <- get_antigen_pairs(df$name)
  combined_df <- data.frame()
  for (sample in unique(df$sample)) {
    repeated <- get_repeated_from_sample(sample)
    for (row in 1:nrow(ag_pair)) {
      # cat(stri_c("sample: ", sample, "; df$sample: ", df$sample,
      #            ";\n df$name: ", df$name, ";\n ag_pair$ai[row]: ",
      #            ag_pair$a1[row], "\n\n"))
      if (!any(df$name == ag_pair$a1[row] |
               df$name == ag_pair$a2[row]))
        next
      if (any(df$name == ag_pair$a1[row])) {
        row_order <- min(df$row_order[df$sample == sample &
                                        df$name == ag_pair$a1[row]])
      } else {
        row_order <- min(df$row_order[df$sample == sample &
                                        df$name == ag_pair$a2[row]])
      }
      report_date <- df$report_date[df$sample == sample][1]
      sample_date <- df$sample_date[df$sample == sample][1]
      snprc_id <- stri_trim_both(df$animal_id[df$sample == sample][1])
      wells <- stri_c(unique(stri_trim_both(df$wells[df$sample == sample])),
                      collapse = ",")
      result_str <- stri_c(
        df[df$sample == sample &
             df$name == ag_pair$a1[row], "result"],
        df[df$sample == sample &
             df$name == ag_pair$a2[row], "result"], ignore_null = TRUE)
      if (is.na(result_str))
        next
      if (length(result_str) == 0)
        next

      if (length(result_str) == 1) {
        # cat(stri_c("file: ", file, "; row: ", row,
        #             "; result_str: ", result_str,
        #            "; sample: ", sample,
        #            "; snprc_id: ", snprc_id,
        #            "; sample_date: ", sample_date,
        #            "; repeated: ", repeated,
        #            "; agent: ", ag_pair$agent[row], "\n"))
        combined_df <- rbind(
          combined_df,
          data.frame(
            row_order = row_order,
            file_name = basename(file),
            sample = sample,
            snprc_id = snprc_id,
            sample_date = sample_date,
            report_date = report_date,
            repeated = repeated,
            wells = wells,
            agent = ag_pair$agent[row],
            assay_value = switch(
              result_str,
              "PP" = "P",
              "P" = "P",
              "NP" = "I",
              "PN" = "I",
              "NN" = "N",
              "N" = "N"
            ),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  #controls <- c("high", "low", "nhpcnt(icl)", "diluent", "cnt", "mac")
  combined_df <- combined_df[!(
    stri_detect_fixed(tolower(combined_df$sample), "icl") |
      stri_detect_fixed(tolower(combined_df$sample), "low") |
      stri_detect_fixed(tolower(combined_df$sample), "high") |
      stri_detect_fixed(tolower(combined_df$sample), "mac")
  ), ]
  combined_df$row_order <- 1:nrow(combined_df)
  combined_df$snprc_id <- remove_bad_animal_ids(conn, combined_df$snprc_id)
  combined_df$species <- get_luminex_species(conn, combined_df$snprc_id)
  combined_df <- add_sqlmed_codes(combined_df, "GIAVEDONI")
  combined_df$plate_id <- get_file_ids(combined_df$file_name)
  birth_date_df <- add_birth_date(conn, data.frame(
    id = unique(combined_df$snprc_id)))
  birth_date_df$birth_date <- format(birth_date_df$birth_date,
                                     format = "%Y-%m-%d")

  combined_df <- merge(combined_df,
                       birth_date_df, by.x = "snprc_id",
                       by.y = "id", all.x = TRUE)
  colony_df <-  data.frame( id = combined_df$snprc_id,
                            colony_date = combined_df$sample_date)
  colony_df <- colony_df[!duplicated(colony_df$id), ]
  combined_df <- merge(combined_df,
                       add_colony(conn, colony_df),
                       by.x = "snprc_id", by.y = "id", all.x = TRUE)
  combined_df[order(combined_df$row_order),
              c("file_name", "plate_id", "species", "birth_date", "colony",
                "sample", "snprc_id",
                "sample_date", "report_date", "repeated", "wells",
                "procedure_name", "procedure_id",
                "test_name", "test_id", "agent", "assay_value")]
}
