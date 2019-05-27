#' Returns the Ids in right blank filled and upper case animal Ids.
#'
#' @param ids character vector of animal Ids that may need to be transformed
#' into the proper Id format.
#' @param upper logical indicator of whether ids are to be forced to upper case
#' @examples
#' new_ids <- blank_fill_ids(c("12345", "1X1234", "1234","2 3456"))
#' @import stringi
#' @export
blank_fill_ids <- function(ids, upper = TRUE) {
  if (upper)
    ids <- toupper(stri_trim_both(ids))
  if (class(ids) == "factor")
    ids <- as.character(ids)
  for (i in seq_along(ids)) {
    if (is.na(ids[i]))
      next

    if (stri_length(ids[i]) > 6 | stri_length(ids[i]) < 3)
      warning(stri_c("Id size out of range. i is ", i,
                     " stri_length(ids[[i]]) is ",
                     stri_length(ids[[i]]),
                     " and Id is ", ids[[i]]))
    else
      ids[[i]] <- sprintf("%6s", ids[[i]])
  }
  ids
}
#' Returns database connection object
#'
#' @param spf_dsn DSN for the SPF user
#' @import RODBC
spf_odbcConnect <- function(spf_dsn) {
 odbcConnect(spf_dsn)
}
#' Get dataframe of animals where the most recent assay was not negative.
#'
#' A PCR assay trumps an non-PCR assay so that if a PCR assay is negative and
#' and non-PCR assay is non-negative, the animal is negative for that agent.
#'
#' @return A dataframe with assays whose last value are non-negative.
#'
#' @param sorted_df dataframe sorted values of all assays.
#' @import stringi
#' @import sqldf
#' @export
get_still_pos <- function(sorted_df) { # renamed still_pos()
  assay_pairs_df <- get_assay_pairs()
  still_pos_df <- data.frame()
  for (i in seq_along(assay_pairs_df$agent)) {
    ab_assay <- assay_pairs_df$ab_assay[i]
    pcr_assay <- assay_pairs_df$pcr_assay[i]
    last_assay_is_nonnegative <-
      sqldf(stri_c(
        "select max(n_neg_df.sample_date),
        n_neg_df.* from n_neg_df
        where not exists(select 1 from sorted_df
        where sorted_df.sample_date > n_neg_df.sample_date
        and n_neg_df.id = sorted_df.id
        and sorted_df.`", ab_assay, "` != 'N')"))
    results <- sqldf(stri_c(
      "select * from last_assay_is_nonnegative a
      where not exists(select 1 from sorted_df where sorted_df.`",
      pcr_assay, "` = 'N' and sorted_df.id = a.id)"))
    still_pos_df <-
      rbind(still_pos_df,
            data.frame(agent = rep(assay_pairs_df$agent[i], nrow(results)),
                       results, check.names = FALSE))
  }
  still_pos_df
}
#' Returns a dataframe, with sets of assays with an antibody assay (ab_assay)
#' and a PCR based assay (pcr_assay) for each agent.
#'
get_assay_pairs <- function() {
  data.frame(agent = c("SIV", "SRV", "STLV-1"),
             ab_assay = c("SIV AB", "SRV AB", "STLV-1 AB"),
             pcr_assay = c("SIV PCR", "SRV PCR",
                           "STLV-1 BY PCR"),
             stringsAsFactors = FALSE) # no HERPES test
}
#' Returns dataframe with raw mfi values from the raw section of the Excel
#' file.
#'
#' @param conn database connection object
#' @param df character vector with line from the Excel file worksheet
#' having only the result table records
#' @param file character vector of length one with filename
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @return \code{raw_mfi_df} dataframe
#' @import reshape2
#' @import stringi
#' @export
get_raw_mfi_df <- function(conn, df, file, run_props, run_error) {
  wide_raw_df <- data.frame(
    file = rep(file, nrow(df)),
    sample = df$sample,
    animal_id = get_id_from_sample(conn, df$sample),
    sample_date = get_date_from_sample(conn, df$sample),
    report_date = get_report_date_from_filename(file, run_props, run_error),
    df[ , -1],
    stringsAsFactors = FALSE, check.names = FALSE
  )
  raw_df <- melt(wide_raw_df,
                 id.vars = c("file", "sample", "animal_id",  "sample_date",
                             "report_date", "wells"),
                 measure.vars = names(wide_raw_df)[
                   7:length(names(wide_raw_df))],
                 variable.name = "name",
                 value.name = "val", factorsAsStrings = TRUE)
  raw_df$wells <- sapply(raw_df$wells, function(well) {
    if (stri_sub(well, stri_length(well), stri_length(well)) == ",") {
      well <- stri_sub(well, 1, stri_length(well) - 1)
    } else {
      well
    }})

  raw_df$val <- suppressWarnings(as.numeric(raw_df$val))
  raw_df
}
#' Returns a sorted dataframe with First occurrances appearing in
#' chronological order followed by all subsequent assay results.

#' Sample results based on pooled samples are included. If an animal is lost
#' to followup due to leaving the institution or death a record indicating that
#' event and date is the last record in the group.
#'
#' @param followup_df dataframe with all records.
#' @return sorted \code{followup_df} as \code{sorted_df}
#' @import stringi
#' @export
sort_occurances <- function(followup_df) {
  f_df <- followup_df[stri_detect_fixed(followup_df$flag, "FIRST"), ]
  f_df <- f_df[order(f_df$sample_date, f_df$report_date), ]
  c_df <- followup_df[stri_detect_fixed(followup_df$flag, "Confirm"), ]
  c_df <- c_df[order(c_df$sample_date, c_df$report_date), ]
  e_df <- followup_df[stri_detect_fixed(followup_df$flag, "Exit"), ]
  e_df <- e_df[order(e_df$sample_date, e_df$report_date), ]
  p_df <- followup_df[stri_detect_fixed(followup_df$flag, "Pooled"), ]
  p_df <- p_df[order(p_df$sample_date, p_df$report_date), ]

  sorted_df <- data.frame()
  for (i in seq_along(f_df$flag)) {
    sorted_df <- rbind(sorted_df, f_df[i, ])
    tmp_df <- c_df[c_df$id == f_df$id[i], ]
    for (j in seq_along(tmp_df$flag)) {
      sorted_df <- rbind(sorted_df, tmp_df[j, ])
    }
    tmp_df <- p_df[p_df$id == f_df$id[i], ]
    for (j in seq_along(tmp_df$flag)) {
      sorted_df <- rbind(sorted_df, tmp_df[j, ])
    }
    sorted_df <- rbind(sorted_df, e_df[e_df$id == f_df$id[i], ])
  }
  drop_cols <- character(0)
  for (col in names(sorted_df)) {
    if (all(is.na(sorted_df[ , col])))
      drop_cols <- c(drop_cols, col)
  }
  sorted_df <- sorted_df[, !(names(sorted_df) %in% drop_cols)]
  sorted_df
}
#' Returns report date in yyyy-mm-dd format as derived from the file names.
#'
#' @param filenames character vector with one or more file names. The
#' file names of the Excel files have embedded report dates.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#'
#' @import stringi
#' @export
get_report_date_from_filename <- function(filenames, run_props, run_error) {
  dates <- rep(NA, length(filenames))
  tmp_names <-
    basename(filenames)[!stri_detect_fixed(tolower(basename(filenames)),
                                           "spfcolony")]
  if (any(stri_detect_regex(tolower(tmp_names), "plate [0-9]+"))) {
    tmp_dates <- sapply(stri_split_regex(tolower(tmp_names), "plate [0-9]+"),
                        function(x) {x[[2]]})
    tmp_dates <-
      stri_sub(tmp_dates,
               stri_locate_first_regex(tmp_dates,
                                       pattern = "[0-9]{2}-[0-9]{2}-[0-9]{2,4}"))
    dates[!stri_detect_fixed(tolower(basename(filenames)), "spfcolony")] <-
      tmp_dates
    dates <- mdy_to_yyyymmdd(dates, sep = "-")
  } else {
    triggerError(run_props, run_error,
                 "The file name is missing the 'plate #' section.")
  }
  dates
}
#' Returns pooled_df dataframe with bleed_date replaced by sample_date.
#'
#' @param pooled_df dataframe with bleed_date in yyyymmdd format.
#' @param sep character vector of length one having the character(s) to be used
#' @return \code{pooled_df} dataframe with \code{sample_date} in yyyy-mm-dd
#' format.
#' @import stringi
#' @export
pooled_bleed_date_to_sample_date <- function(pooled_df, sep = "-") {
  pooled_df$sample_date <-
    stri_datetime_format(
      stri_datetime_parse(pooled_df$bleed_date, format = "uuuuMMdd"),
      format = "uuuu-MM-dd")
  # pooled_df$sample_date <- stri_c(stri_sub(pooled_df$bleed_date, 1, 4),
  #                                 rep(sep, nrow(pooled_df)),
  #                                 stri_sub(pooled_df$bleed_date, 5, 6),
  #                                 rep(sep, nrow(pooled_df)),
  #                                 stri_sub(pooled_df$bleed_date, 7, 8)
  # )
  pooled_df$bleed_date <- NULL
  pooled_df
}
#' Returns character vector of dates where entities of mm/dd/yy or
#' mm-dd-yy are converted to yyyymmdd.
#'
#' @param dates character vector of potential dates
#' @param sep character vector of length one having the character(s) to be used
#' to separate elements with each date.
#' @return date with format of yyyymmdd in character vector
#' @import stringi
#' @export
mdy_to_yyyymmdd <- function(dates, sep = "") {
  dates <- stri_replace_all_fixed(dates, "/", "-")
  dates <- sapply(dates, function(.date) {

    if (!is.na(.date) & stri_detect_fixed(.date, "-")) {
      tmp <- stri_split_fixed(.date, "-")[[1]]
      if (length(tmp) == 3) {
        tmp[1] <- stri_sub(stri_c("0", tmp[1], collapse = ""), -2, 3)
        tmp[2] <- stri_sub(stri_c("0", tmp[2], collapse = ""), -2, 3)
        if (stri_length(tmp[3]) == 2) { # Not robust code because of fixed date
          if (as.integer(tmp[3]) < 50) {
            tmp[3] <- stri_c("20", tmp[3])
          } else {
            tmp[3] <- stri_c("19", tmp[3])
          }
        }
        .date <- stri_c(tmp[3], sep, tmp[1], sep, tmp[2], collapse = "")
      }
      .date
    }
    .date
  })
  names(dates) <- NULL
  dates
}
#' Returns ``followup'' assay results, which are groups of assay results for an
#' animal beginning with the first non-negative result and all subsequent
#' results.
#'
#' @param dsn character with the dataset name
#' @param start_date character with start date in mm-dd-yyyy format.
#' @import RODBC
#' @import stringi
#' @export
get_followup <- function(dsn, start_date = "1-1-2013") {
  sql_txt <- stri_c(
    "select CASE WHEN f.flag = '** Exited colony **' THEN f.flag + ': ' + ",
    "f.procedure_name ELSE f.flag END AS flag,
    f.id ,
    f.sample_id ,
    f.sample_date ,
    f.report_date ,
    f.procedure_name ,
    f.source ,
    f.[HERPES B VIRUS] ,
    f.[SIV AB] ,
    f.[SRV AB] ,
    f.[STLV-1 AB] ,
    f.[Herpes B Surrogate M],
    f.[HVP-2] ,
    f.[SIV WB] ,
    f.[SIV PCR] ,
    f.[SRV1 WB] ,
    f.[SRV2 WB] ,
    f.[SRV PCR] ,
    f.[STLV-1 AB PDL] ,
    f.[STLV WB] ,
    f.[STLV-1 BY PCR]
    FROM dbo.f_select_positive_surveillance() AS f
    Where f.sample_date > = '", start_date, "'
    ORDER BY f.id ASC, f.report_date asc")
  conn <- spf_odbcConnect(dsn)
  followup_df <- sqlQuery(conn, sql_txt, na.strings = "",
                          stringsAsFactors = FALSE)
  odbcClose(conn)
  followup_df$flag <-
    stri_trim_both(stri_replace_all_fixed(followup_df$flag, "*", ""))
  followup_df$`SRV PCR`[
    followup_df$`SRV PCR` == "P" &
      stri_detect_fixed(followup_df$flag, "Pooled")] <- "P (pooled)"
  followup_df$`SRV PCR`[
    followup_df$`SRV PCR` == "I" &
      stri_detect_fixed(followup_df$flag, "Pooled")] <- "I (pooled)"
  followup_df$`SRV PCR`[
    followup_df$`SRV PCR` == "N" &
      stri_detect_fixed(followup_df$flag, "Pooled")] <- "N (pooled)"
  followup_df
}
#' Returns a dataframe with pooled_df merged with pdl_df by pooled_id with
#' pdl_animal_id. All pooled records are retained.
#'
#' @param pooled_df dataframe with pooled samples
#' @param pdl_df dataframe with PDL results.
#' @export
merge_pooled_and_pdl <- function(pooled_df, pdl_df) {
  pooled_df <- merge(pooled_df, pdl_df, by.x = "pool_id",
                     by.y = "pdl_animal_id", all.x = TRUE)
  pooled_df <- pooled_df[!is.na(pooled_df$results), ]
  pooled_df$sample_date <- pooled_df$sample_date.x
  pooled_df$sample_date.x <- NULL
  pooled_df$sample_date.y <- NULL
  pooled_df
}

#' Returns assay results for individual animals based on assay results of pooled
#' samples.
#'
#' @param followup_df dataframe with animal Ids to use in selected pool sample
#' results.
#' @import RODBC
#' @import stringi
#' @export
get_followup_pool_samples <- function(followup_df) {
  pooled_df <- get_pooled_sample_df("../data/pool_definitions/")
  pooled_df <- pooled_df[pooled_df$snprc_id %in% unique(followup_df$id) &
                           pooled_df$blood_received == "yes", ]
  pooled_df <- pooled_bleed_date_to_sample_date(pooled_df, sep = "-")
  conn <- spf_odbcConnect("frogstar-vortex-animal-sa")
  sql_txt <- stri_c(
    "select * from pdl_results where substring(pdl_animal_id, 1, 1) = 'V'"
  )
  pdl_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  odbcClose(conn)
  pdl_df <- pdl_df[ , c("pdl_animal_id", "sample_date", "report_date",
                        "procedure_name", "test_id",
                        "test_name", "results")]
  pdl_df$results[toupper(stri_sub(pdl_df$results, 1, 1)) == "P"] <- "P"
  pdl_df$results[toupper(stri_sub(pdl_df$results, 1, 1)) == "I"] <- "I"
  pdl_df$results[toupper(stri_sub(pdl_df$results, 1, 1)) == "N"] <- "N"
  pooled_df <- merge_pooled_and_pdl(pooled_df, pdl_df)

  pooled_df <- data.frame(flag = rep("Pooled SRV PCR", nrow(pooled_df)),
                          id = pooled_df$snprc_id,
                          sample_id = stri_c(pooled_df$snprc_id,
                                             rep(" - ", nrow(pooled_df)),
                                             pooled_df$sample_date),
                          sample_date = pooled_df$sample_date,
                          report_date = pooled_df$report_date,
                          procedure_name = pooled_df$procedure_name,
                          source = rep("PDL", nrow(pooled_df)),
                          `HERPES B VIRUS` = rep(NA, nrow(pooled_df)),
                          `SIV AB` = rep(NA, nrow(pooled_df)),
                          `SRV AB` = rep(NA, nrow(pooled_df)),
                          `STLV-1 AB` = rep(NA, nrow(pooled_df)),
                          `Herpes B Surrogate M` = rep(NA, nrow(pooled_df)),
                          `HVP-2` = rep(NA, nrow(pooled_df)),
                          `SIV WB` = rep(NA, nrow(pooled_df)),
                          `SIV PCR` = rep(NA, nrow(pooled_df)),
                          `SRV1 WB` = rep(NA, nrow(pooled_df)),
                          `SRV2 WB` = rep(NA, nrow(pooled_df)),
                          `SRV PCR` = as.character(pooled_df$results),
                          `STLV-1 AB PDL` = rep(NA, nrow(pooled_df)),
                          `STLV WB` = rep(NA, nrow(pooled_df)),
                          `STLV-1 BY PCR` = rep(NA, nrow(pooled_df)),
                          stringsAsFactors = FALSE, check.names = FALSE)
  pooled_df
}
#' Returns the file name of the Excel file created which contains the
#' dataframes provided as an arguments to the function.
#'
#' @param sorted_df a datafrome containing the sorted followup assay results.
#' @param still_pos_df a datafrome containing the sorted followup assay results
#' by animal where the assay results are still non-negative.
#' @return name of Excel file saved containing the sorted followup dataframe
#' (\code{sorted_df}).
#' @import lubridate
#' @import rmsutilityr
#' @export
save_excel_sheet <- function(sorted_df, still_pos_df) {
  prefix <- stri_c(
    stri_replace_all_fixed(stri_replace_all_fixed(now(), " ", "_"), ":", "-"),
    "_")
  col_names <- remove_strings(names(sorted_df), c("sample_id", "report_date",
                                                  "procedure"))
  excel_file <- stri_c("../reports/", prefix, "followup", ".xlsx")
  create_wkbk(excel_file, list(sorted_df[ , col_names], still_pos_df),
              c("followup", "still_positive"))
  excel_file
}
#' Returns pdl_df with columns renamed, added and ordered to prepare the
#' dataframe for inserting into the pdl_result table.
#'
#' @param pdl_df dataframe as it comes from read.csv()
#' @import rmsutilityr
#' @import stringi
#' @export
fix_pdl_csv_columns <- function(pdl_df) {
  names(pdl_df) <- c("file_name", "print_date_tm", "order_pk", "client",
                     "bill_po", "request_num", "received_date", "report_date",
                     "order_comment", "var_9", "l_name", "f_name",
                     "sample", "pdl_animal_id", "pdl_species", "sample_type",
                     "sample_date", "comment", "var_3", "var_4", "panel",
                     "var_5", "test_type", "results", "test_comment", "var_6")
  pdl_df$report_contact <- stri_c(pdl_df$l_name, rep(", ", nrow(pdl_df)),
                                  pdl_df$f_name)
  pdl_df$test_comment <- stri_c(pdl_df$test_comment, rep(", ",
                                                         nrow(pdl_df)),
                                pdl_df$var_6)
  pdl_df$var_6 <- NULL
  pdl_df$snprc_id <- suppressWarnings(
    sapply(pdl_df$pdl_animal_id, function(id) {get_snprc_id(id)}))
  pdl_df$pooled <- sapply(pdl_df$pdl_animal_id, function(id) {
    pooled_Y_or_N(id)})
  pdl_df <- add_sqlmed_codes(pdl_df, "PDL")
  pdl_df <- pdl_df[ , c("file_name", "order_pk", "print_date_tm",
                        "bill_po", "request_num", "report_contact",
                        "received_date", "report_date", "order_comment",
                        "sample", "pdl_animal_id", "snprc_id",
                        "pooled", "pdl_species",  "sample_type",
                        "sample_date", "comment", "test_type",
                        "results", "test_comment", "procedure_id",
                        "procedure_name", "test_id", "test_name")]#,
  #    "var_9", "l_name", "f_name", "client",
  #    "var_3", "var_4",
  #    "var_5")]
  pdl_df
}
#' Returns a dataframe with a row for each bad CSV file with the original
#' name and the renamed file
#'
#' Some of the files from PDL have text trailing the CSV portion of the file.
#' This routine removes files from the end until a record that starts with
#' a token of all integers is found. This is the order primary key.
#' The bad file has been replaces with a cleaned CSV
#' file having the original name.
#' @param path character vector of length one with the file path
#' @param file_names character vector of the file names to be read
#' @param orig_prefix character string with the prefix to be put on the
#' renamed bad file.
#' @import stringi
#' @export
replace_bad_csv_files <- function(path, file_names, orig_prefix = "org") {
  bad_csv_files_df <- data.frame()
  for (file in file_names) {
    replace <- FALSE
    file_txt <- scan(file = stri_c(path, file), what = "character", sep = "\n",
                     quiet = TRUE)
    for (i in rev(seq_along(file_txt)[-1])) {
      #cat(stri_c("Line: ", i, " ", file_txt[i], "\n\n"))
      if (stri_detect_regex(stri_sub(file_txt[i], 2, length = 5),
                            "[[a-zA-Z]]")) {
        last <- i - 1
        replace <- TRUE
      } else {
        break
      }
    }
    if (replace) {
      file.rename(from = stri_c(path, file), to = stri_c(path,
                                                         orig_prefix, file))
      write(file_txt[1:last], file = stri_c(path, file))
      bad_csv_files_df <-
        rbind(bad_csv_files_df,
              data.frame(original_name = stri_c(path, file),
                         renamed_file = stri_c(path, orig_prefix, file)))
    }
  }
  bad_csv_files_df
}
#' Returns pdl data from their CSV files given a list of filenames and the
#' relative or absolute path.
#'
#' @param file_names character vector with the files to be read
#' @param path character string with the path
#' @import stringi
#' @export
get_pdl_csv_data <- function(file_names, path) {
  pdl_df <- data.frame()
  for (file in as.character(file_names)) {
    file_df <- utils::read.csv(stri_c(path, file), stringsAsFactors = FALSE,
                        check.names = FALSE, fill = TRUE, na.strings = "N/A")
    # cat(stri_c("Read: ", file, ", ", vector2string(names(file_df),
    #                                                SS = "', '"), "\n\n"))
    file_df <- file_df[ , 1:24]
    pdl_df <-
      rbind(pdl_df, data.frame(file = rep(basename(file), nrow(file_df)),
                               print_date_tm = rep("", nrow(file_df)), file_df,
                               stringsAsFactors = FALSE, check.names = FALSE))
  }
  pdl_df <- fix_pdl_csv_columns(pdl_df)

  pdl_df <-
    pdl_df[!stri_detect_fixed(pdl_df$test_type, pattern = "ABSCN-5"), ]
  pdl_df[is.na(pdl_df)] <- ""
  pdl_df
}
#' Returns TRUE if the creation of the spf_samples table was successful.
#'
#' Has the side effect of creating an SQL Server database table to hold the
#' list of SPF samples.
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
create_spf_samples_tbl <- function(conn) {
  sql_txt <- stri_c(
    "CREATE TABLE animal.dbo.spf_samples
    (spf_sample_id int IDENTITY(1,1) NOT NULL,
    file_name varchar(40) NOT NULL,
    snprc_id char(6) NOT NULL,
    cage numeric(6,2),
    bleed_date date,
    blood_expected char(1),
    blood_received char(1),
    pool_id varchar(20),
    pooled char(1),
    OBJECT_ID uniqueidentifier DEFAULT (newid()) NOT NULL,
    USER_NAME varchar(128) DEFAULT (user_name()) NOT NULL,
    ENTRY_DATE_TM datetime DEFAULT (getdate()) NOT NULL,
    TIMESTAMP timestamp,
    PRIMARY KEY (spf_sample_id))")
  status <- sqlQuery(conn, sql_txt)
  if (length(status) != 0) {
    print(stri_c("Creation of database table 'spf_samples' failed with error
                 message of: ", status))
    return(FALSE)
  } else {return(TRUE)}
}
#' Returns TRUE if the creation of the pdl_results table was successful.
#'
#' Has the side effect of creating an SQL Server database table to hold the
#' results received from PDL.
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
create_pdl_results_tbl <- function(conn) {
  sql_txt <- stri_c(
    "CREATE TABLE animal.dbo.pdl_results
    (pdl_result_id int IDENTITY(1,1) NOT NULL,
    file_name varchar(30) NOT NULL,
    order_pk int,
    print_date_tm datetime,
    bill_po varchar(10),
    request_num varchar(10),
    report_contact varchar(20),
    received_date date,
    report_date date,
    order_comment varchar(255),
    sample int NOT NULL,
    pdl_animal_id varchar(20) NOT NULL,
    snprc_id char(6) NOT NULL,
    pooled char(1) NOT NULL,
    pdl_species varchar(10) NOT NULL,
    sample_type varchar(20) NOT NULL,
    sample_date date NOT NULL,
    comment varchar(40),
    test_type varchar(20) NOT NULL,
    procedure_id int NOT NULL,
    procedure_name varchar(40) not NULL,
    test_id int NOT NULL,
    test_name varchar(20) NOT NULL,
    results varchar(20) NOT NULL,
    test_comment varchar(30) NOT NULL,
    OBJECT_ID uniqueidentifier DEFAULT (newid()) NOT NULL,
    USER_NAME varchar(128) DEFAULT (user_name()) NOT NULL,
    ENTRY_DATE_TM datetime DEFAULT (getdate()) NOT NULL,
    TIMESTAMP timestamp,
    PRIMARY KEY (pdl_result_id))")
  status <- sqlQuery(conn, sql_txt)
  if (length(status) != 0) {
    print(stri_c("Creation of database table 'pdl_result' failed with error
                 message of: ", status))
    return(FALSE)
  } else {return(TRUE)}
}
#' Returns TRUE if the creation of the pdl_result table was successful.
#'
#' Has the side effect of creating an SQL Server database table to hold the
#' results received from PDL.
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
create_luminex_screen_results_tbl <- function(conn) {
  sql_txt <- stri_c(
    "CREATE TABLE animal.dbo.luminex_screen_results
    (luminex_screen_tid int IDENTITY(1,1) NOT NULL,
    file_name varchar(100) NOT NULL,
    plate_id varchar(6) NOT NULL,
    sample varchar(30) NOT NULL,
    species varchar(20),
    snprc_id varchar(6) NOT NULL,
    sample_date date,
    report_date date,
    repeated varchar(1),
    agent varchar(20) NOT NULL,
    procedure_id int NOT NULL,
    procedure_name varchar(40) not NULL,
    test_id int NOT NULL,
    test_name varchar(20) NOT NULL,
    assay_value varchar(15) NOT NULL,
    OBJECT_ID uniqueidentifier DEFAULT (newid()) NOT NULL,
    USER_NAME varchar(128) DEFAULT (user_name()) NOT NULL,
    ENTRY_DATE_TM datetime DEFAULT (getdate()) NOT NULL,
    TIMESTAMP timestamp,
    PRIMARY KEY (luminex_screen_tid))")
  status <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  if (length(status) != 0) {
    print(stri_c("Creation of database table 'luminex_screen_results' failed
                  with error message of: ", status))
    return(FALSE)
  } else {return(TRUE)}
}
#' Returns the number of records inserted into spf_samples
#'
#' @param conn database connection object
#' @param spf_samples_df dataframe with samples
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import RODBC
#' @import stringi
#' @export
insert_spf_samples <- function(conn, spf_samples_df, run_props, run_error) {
  spf_samples_df$file_name <- basename(as.character(spf_samples_df$file_name))
  spf_samples_df$cage[is.na(spf_samples_df$cage)] <- "NULL"
  spf_samples_df$pool_id[is.na(spf_samples_df$pool_id)] <- ""
  spf_samples_df$pooled[is.na(spf_samples_df$pooled)] <- ""
  spf_samples_df$blood_expected[is.na(spf_samples_df$blood_expected)] <- ""
  spf_samples_df$blood_received[is.na(spf_samples_df$blood_received)] <- ""
  row_count <- 0
  for (i in 1:nrow(spf_samples_df)) {
    rc <- spf_samples_df[i, ]
    sql_txt <- stri_c(
      "insert into spf_samples (", vector2string(names(spf_samples_df),
                                                 SS = ", "),
      ") VALUES ('",
      rc$file_name, "', '",
      rc$snprc_id, "', ",
      rc$cage, ", '",
      rc$bleed_date, "', '",
      rc$blood_expected, "', '",
      rc$blood_received, "', '",
      rc$pool_id, "', '",
      rc$pooled, "')")
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0) {
      if (!status == "No Data")
        triggerError(run_props, run_error,
                     msg = stri_c("Insert into spf_samples failed status: ",
                                  status))
    }
    row_count <- row_count + as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  if (row_count != nrow(spf_samples_df)) {
    triggerError(run_props, run_error,
                 msg = (stri_c("Number of rows inserted into spf_samples (",
                               row_count,
                               ") is not the number of samples found (",
                               nrow(spf_samples_df), ".")))
  }
  row_count
}

#' Returns the number of records inserted into pdl_results
#'
#' @param conn database connection object
#' @param pdl_df dataframe with PDL results
#' @import RODBC
#' @import stringi
#' @export
insert_pdl_results <- function(conn, pdl_df) {
  pdl_df$file_name <- basename(pdl_df$file_name)
  row_count <- 0
  for (i in 1:nrow(pdl_df)) {
    rc <- pdl_df[i, ]
    sql_txt <- stri_c(
      "insert into pdl_results (", vector2string(names(pdl_df), SS = ", "),
      ") VALUES ('", rc$file_name, "', ", rc$order_pk, ", '", rc$print_date_tm,
      "', '", rc$bill_po, "', '", rc$request_num, "', '",
      rc$report_contact, "', '",
      rc$received_date, "', '", rc$report_date, "', '", rc$order_comment, "', ",
      rc$sample, ", '",
      vector2string(rc[ , c(11:ncol(pdl_df))], SS = "', '"), "')")
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0) {
      if (!status[[1]] == "No Data") {
        stop(stri_c("insert into pdl_results failed status: ", status))
        odbcClose(conn)
      }
    }
    row_count <- row_count + as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  row_count
}
#' Returns the number of saved records
#'
#' @param conn database connection object
#' @param r_df dataframe containing the melted version of the individual bead
#' @param table optional character variable with name of table to insert into.
#' assay
#' results.
#' @import RODBC
#' @import stringi
#' @export
insert_combined_assay_results <- function(conn, r_df,
                                          table = "luminex_screen_results") {
  r_df <- r_df[!is.na(r_df$snprc_id), ]
  r_df$file_name <- basename(r_df$file_name)
  row_count <- 0
  for (i in seq_along(r_df$snprc_id)) {
    r_df[i, ][is.na(r_df[i, ])] <- ""
    sql_txt <- stri_c(
      "INSERT INTO ", table, " (
      file_name, plate_id, sample, species, snprc_id, sample_date,
      report_date, repeated, agent,
      procedure_id, procedure_name, test_id, test_name, assay_value)
      values ('", vector2string(as.character(
        r_df[i, c("file_name", "plate_id", "sample", "species", "snprc_id",
                  "sample_date", "report_date",
                  "repeated", "agent", "procedure_id", "procedure_name",
                  "test_id", "test_name", "assay_value")]), "', '"), "') ")
    #cat(stri_c(sql_txt, "\n"))
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0) {
      if (!status == "No Data")
        stop(stri_c("insert into ", table, " failed status: ",
                    status))
    }
    row_count <- row_count + as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  row_count
}
#' Returns character vector of column names base on type requested.
#'
#' type can be one of c("bead", "syntactic_bead", "summary",
#' "syntactic_summary")
#' @param type character vector of length one having a column type
#' @import rmsutilityr
#' @import stringi
#' @export
get_col_names <- function(type) {
  valid_cols <- c("raw", "syntactic_raw", "bead", "syntactic_bead", "summary",
                  "syntactic_summary", "agent")
  if (!any(type %in% valid_cols)) {
    stop(stri_c(type, " is not a valid column type name. Must be one of ",
                get_and_or_list(valid_cols), "."))
  }
  control_cols <- c("Human IgG", "wBAC", "Goat anti-human IgG")
  antigen_cols <- get_test_antigens()
  raw_cols <- c(antigen_cols, control_cols)
  bead_cols <- c("Filename", "Animal ID", antigen_cols)
  agent_cols <- unique(get_agents_and_antigen_pairs()$agent)
  summary_cols <- c("Filename", "Animal.ID", agent_cols)

  col_names_list <- list(
    raw = raw_cols,
    sytactic_raw = make.names(raw_cols),
    bead = bead_cols,
    syntactic_bead = make.names(bead_cols),
    summary = summary_cols,
    syntactic_summary = make.names(summary_cols),
    agent = agent_cols,
    syntactic_agent = make.names(agent_cols))
  col_names_list[type][[1]]
}
#' Returns a character vector of names of usable files.
#'
#' Usable files are those from "Report plate 55 03-21-2014.xlsx" forward
#' because of large fluxuations in format.
#'
#' @param path file path to directory containing files to be read.
#' @import stringi
#' @export
get_usable_files <- function(path) {
  files <- list.files(path = path, pattern = "*.xlsx", full.names = TRUE,
                      ignore.case = TRUE)
  files <- files[!stri_detect_fixed(files, "~$R")]
  usable_file_patterns <- stri_c("Report plate ", 55:300)
  lc_files <- tolower(files)
  report_files <- as.character(sapply(tolower(usable_file_patterns),
                               function(pattern) {
    files[stri_detect_fixed(lc_files, pattern = pattern)][1]}))
  spf_files <- files[stri_detect_fixed(lc_files,
                                       pattern = "spf colony testing")]
  files <- unique(c(report_files, spf_files))
  files[!is.na(files)]
}

#' Returns dataframe with means of replicated samples
#'
#' @param raw_mfi_df dataframe with raw mfi reading
#' @import stringi
#' @export
get_mean_raw_mfi <- function(raw_mfi_df) {
  df <- data.frame(row_order = 1:nrow(raw_mfi_df), raw_mfi_df)
  mean_raw_mfi <- data.frame()
  col_names <- c("row_order", "file", "sample", "animal_id", "sample_date",
                 "report_date", "wells", "name", "val")
  for (sample in unique(df$sample)) {
    for (name in unique(df$name)) {
      tmp_df <- df[df$name == name & df$sample == sample, ]
      val <- mean(tmp_df$val)
      wells <- stri_c(tmp_df$well_row, tmp_df$well_col, collapse = ",")
      tmp_df <- tmp_df[1, ]
      tmp_df$wells <- wells
      tmp_df$val <- val
      mean_raw_mfi <- rbind(mean_raw_mfi, tmp_df[ , col_names])
    }
  }
  mean_raw_mfi <- mean_raw_mfi[order(mean_raw_mfi$row_order), -1]
  mean_raw_mfi
}
#' Returns mfi_df dataframe values with wBAC column subtracted from each
#' value and the wBAC and the Human IgG, wBAC and Goat anti-human IgG rows
#' removed from raw_mfi_df
#'
#' @param raw_mfi_df dataframe with raw data
#' @export
subtract_background <- function(raw_mfi_df) {
  df <- data.frame(row_order = 1:nrow(raw_mfi_df), raw_mfi_df)
  samples <- unique(df$sample)
  for (sample in samples) {
    df$val[df$name != "wBAC" & df$sample == sample] <-
      df$val[df$name != "wBAC" & df$sample == sample] -
      df$val[df$name == "wBAC" & df$sample == sample]
  }
  df <- df[order(df$row_order), -1]
  mfi_df <- df[!df$name %in% c("wBAC", "Human IgG",
                               "Goat anti-human IgG"), ]
  mfi_df
}
#' Returns mfi_df with each value divided by the appropriate divisors
#'
#' @param mfi_df dataframe with substracted background
#' @param divisors named numeric vector of divisors
#' @export
apply_divisor <- function(mfi_df, divisors) {
  df <- data.frame(row_order = 1:nrow(mfi_df), mfi_df)
  for (name in names(divisors)) {
    df$val[df$name == name] <- 3 * df$val[df$name == name] /
      divisors[[name]]
    df$sd[df$name == name] <- 3 * df$sd[df$name == name] /
      divisors[[name]]
  }
  df <- df[order(df$row_order), -1]
  df
}
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
#' Returns dataframe of possible antigen pairs and agents
#'
#' @export
get_agents_and_antigen_pairs <- function() {
  data.frame(agent = c("STLV", "SRV", "BV", "BV", "SIV", "SIV", "Measles",
                       "T_cruzi"),
             a1 = c("HTLV-1/2", "SRV-2", "BV glyco B", "rBV glycoB",
                    "SIV gp120", "SIV gp130", "rMeasles", "rChagas"),
             a2 = c("STLV p21", "SRV gp-20", "HVP-2", "HVP-2",
                    "SIV mac", "SIV mac", "rMeasles", "rChagas"),
             stringsAsFactors = FALSE)

  # labkey.selectRows(
  #   baseUrl = attr(session, "baseUrl"),
  #   folderPath = "/SNPRC/Core Facilities/SPF Screen Workflow",
  #   schemaName = "lists",
  #   queryName = "agent_antigen_pairs",
  #   viewName = "",
  #   colFilter = NULL,
  #   containerFilter = NULL, colNameOpt = "fieldname"
  # )
}
#' #' Makes function to return agents and antigen pairs.
#' #'
#' #' @param session session object from LabKey.
#' #' @import Rlabkey
#' make_get_agents_and_antigen_pairs <- function(session) {
#'   schema_conn <- getSchema(session, "lists")
#'   schema_element <- schema_conn[["agent_antigen_pairs"]]
#'   agents_and_antigen_pairs <- getRows(session, query)
#'   f <- function() {
#'     agents_and_antigen_pairs
#'   }
#' }
#'
#' Returns the list of test antigens
#'
#' Uses \code{get_agents_and_antigen_pairs()} to create the list of unique
#' test antigens
#'
#' @export
get_test_antigens <- function() {
  antigen_pairs <- get_agents_and_antigen_pairs()
  antigens <- c()
  for (i in 1:nrow(antigen_pairs)) {
    antigens <- c(antigens, antigen_pairs$a1[i], antigen_pairs$a2[i])
  }
  unique(antigens)
}
#' Returns  positive antigens based on low control value and a multiplier
#'
#' From e-mail 2015-09-22 13:32 by Luis Giavedoni
#'
#' Mark,
#'
#' When the CNT LOW is below 2.5, we use the CNT LOW score to assign
#' positive reactivity. Any score equal or higher than the CNT LOW will be
#' scored as positive.
#'
#' When the CNT LOW is above 2.5, we use 2.5 as the cut off. Any score equal
#' or higher than 2.5 will be scored as positive.
#'
#' I know that I said that the low score was 3.0, but after review of
#' current Charles River documents, the recommendation is for the score to be
#' 2.5.
#'
#' Regards,
#'
#' Luis
#'
#' Thus, the function used is
#' df$result <- ifelse(df$val >= min(2.5, df$cutoff), "P", "N")
#'
#' \code{SPF Colony Testing Plate 96   06-09-16.xlsx} has an extra low control
#' \code{ICL CNT LOW (dup)}.
#' Controls with \code{stri_detect_fixed(tolower(name), "(dup)") == TRUE}
#'  are ignored.
#'
#' @param d_mfi_df dataframe wih standardized antigen reactivity measures
#' @param mult multiplier used to adjust cutoff level.
#' @import stringi
#' @export
get_positive_antigens <- function(d_mfi_df, mult = 1.0) {
  df <- data.frame(row_order = 1:nrow(d_mfi_df), d_mfi_df)
  df$cutoff <- df$val
  df$result <- rep("NONE", nrow(df))
  for (name in unique(df$name)) {
    for (sample in unique(df$sample)) {
      if (
        length(df$sample[
          stri_detect_fixed(tolower(df$sample), pattern = "low") &
          !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
          !stri_detect_regex(df$sample, pattern = "[0-9]") &
          df$name == name]) == 1) { # A
        df[df$sample == sample & df$name == name, "cutoff"] <-
          df[stri_detect_fixed(tolower(df$sample), pattern = "low") &
               !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
               !stri_detect_regex(df$sample, pattern = "[0-9]") &
               df$name == name, "val"] * mult
      } else if (
        length(df$sample[
          stri_detect_fixed(tolower(df$sample), pattern = "low") &
          !stri_detect_regex(tolower(df$sample), pattern = "lot") &
          !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
          !stri_detect_regex(df$sample, pattern = "[0-9]") &
          df$name == name]) == 1) { # A
        df[df$sample == sample & df$name == name, "cutoff"] <-
        df[stri_detect_fixed(tolower(df$sample), pattern = "low") &
             !stri_detect_regex(tolower(df$sample), pattern = "lot") &
             !stri_detect_regex(tolower(df$sample), pattern = "(dup)") &
             !stri_detect_regex(df$sample, pattern = "[0-9]") &
             df$name == name, "val"] * mult
      } else {
      stop("Low positive control value is missing or there is more than one.")
      }
    }
  }
  df$result <- ifelse(df$val >= pmin(2.5, df$cutoff), "P", "N")
  df <- df[order(df$row_order), -1]
  df
}
#' Returns only good SNPRC animal Ids given a character vector of potential Ids
#'
#' @param conn database connection object
#' @param ids character vector of potential animal Ids.
#' @import animalr
#' @import rmsutilityr
#' @import RODBC
#' @export
remove_bad_animal_ids <- function(conn, ids) {
  ids <- blank_fill_ids(ids)
  bad_ids <- animalr::is_bad_animal_id(conn, ids)
  ids[ids %in% bad_ids] <- NA
  ids

}
#' Returns species desination of "CYNO", "MACAQUE" or "PAPIO" at this time.
#' If the Id does not belong to a cyno, baboon, or rhesus, the arc_species_code
#' is used.
#'
#' @param conn database connection object
#' @param snprc_id character vector with possible animal Ids
#' @import RODBC
#' @import stringi
#' @export
get_luminex_species <- function(conn, snprc_id) {
  ids <- blank_fill_ids(snprc_id[stri_length(snprc_id) <= 6 &
                                      stri_length(snprc_id) >= 4 &
                                      !is.na(snprc_id)])
  id_str <- vector2string(ids, SS = "', '")
  sql_txt <- stri_c("select cd.id, cd.arc_species_code from current_data cd
     where cd.id in ('", id_str, "')")

  result <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  if (!is.data.frame(result))
    stop(stri_c("get_luminex_species failed with the following error:",
                result))
  if (nrow(result) == 0) {
    stop(stri_c("get_luminex_species failed with an empty dataframe.\n"))
  }
  result$species[result$arc_species_code == "PC"] <- "PAPIO"
  result$species[result$arc_species_code == "MM"] <- "MACAQUE"
  result$species[result$arc_species_code == "MF"] <- "CYNO"
  result$species[!result$arc_species_code %in% c("PC", "MM", "MF")] <-
    result$arc_species_code[!result$arc_species_code %in% c("PC", "MM", "MF")]
  result$id <- blank_fill_ids(result$id)
  id_df <- data.frame(row_order = 1:length(snprc_id), id = snprc_id,
                      stringsAsFactors = FALSE)
  df <- merge(id_df, result, by = "id", all.x = TRUE)
  df$species[order(df$row_order)]
}
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
## ## Returns Excel file name of file that has been formatted to highlight
## ## non-negative results in the sheet indicated.
## ##
## ## This highlights "I" and "P" results
## ## @param excel_file character vector with the name of the Excel file
## ## @param .df dataframe being used to guide highlighting. It is the same
## ## one as was used to make the worksheet.
## ## @param sheet_name name of the worksheet to highlight.
## ## @param low_positive_controls_df dataframe with the low positive controls
## ## @importFrom stats complete.cases
## ## @import XLConnect
## ## @export
## format_luminex_results <- function(excel_file, .df,
##                                    sheet_name = "final_result",
##                                    low_positive_controls_df) {
##   ## set-rows
##   ## format cells with highlighting, text wrapping, grey header, and column
##   ## specific borders
##   wb <- loadWorkbook(excel_file)
##   cs_positive <- createCellStyle(wb)
##   cs_indeterminate <- createCellStyle(wb)
##   cs_to_repeat <- createCellStyle(wb) # low positive control wells
##   cs_header <- createCellStyle(wb)
##   cs_border_header <- createCellStyle(wb)
##
##   ## set-wrap
##   setWrapText(cs_positive, wrap = TRUE)
##   setWrapText(cs_indeterminate, wrap = TRUE)
##   setWrapText(cs_to_repeat, wrap = TRUE)
##   setWrapText(cs_header, wrap = TRUE)
##   setWrapText(cs_border_header, wrap = TRUE)
##
##   ## set-fill
##   setFillPattern(cs_positive, fill = XLC$FILL.SOLID_FOREGROUND)
##   setFillForegroundColor(cs_positive, color = XLC$COLOR.ROSE)
##   setFillPattern(cs_indeterminate, fill = XLC$FILL.SOLID_FOREGROUND)
##   setFillForegroundColor(cs_indeterminate, color = XLC$COLOR.CORNFLOWER_BLUE)
##   setFillPattern(cs_to_repeat, fill = XLC$FILL.SOLID_FOREGROUND)
##   setFillForegroundColor(cs_to_repeat, color = XLC$COLOR.LEMON_CHIFFON)
##   setFillPattern(cs_header, fill = XLC$FILL.SOLID_FOREGROUND)
##   setFillPattern(cs_border_header, fill = XLC$FILL.SOLID_FOREGROUND)
##   setFillForegroundColor(cs_header, color = XLC$COLOR.GREY_25_PERCENT)
##   setFillForegroundColor(cs_border_header, color = XLC$COLOR.GREY_25_PERCENT)
##
##
##   ## make-matrices
##   ## Include row offset for column label in Excel sheets
##   positive_row <- matrix(data = rep(1:nrow(.df), each = ncol(.df)),
##                          nrow = nrow(.df),
##                          ncol = ncol(.df), byrow = TRUE) + 1L
##   positive_col <- matrix(data = rep(1:ncol(.df), each = nrow(.df)),
##                          nrow = nrow(.df),
##                          ncol = ncol(.df))
##   ##  positive_row <- positive_row[!is.na(.df)]
##   ##  positive_col <- positive_col[!is.na(.df)]
##   indeterminate_row <- positive_row
##   indeterminate_col <- positive_col
##   to_repeat_row <- positive_row
##   to_repeat_col <- positive_col
##
##   positive <- data.frame(row = positive_row[.df == "P"],
##                          col = positive_col[.df == "P"])
##   indeterminate <- data.frame(row = indeterminate_row[.df == "I"],
##                               col = indeterminate_col[.df == "I"])
##   ## Include row offset for column label in Excel sheets
##   well_rows <- ((1:nrow(.df)) + 1L)[.df[ , "wells"] %in%
##                                low_positive_controls_df$wells]
##   to_repeat <- data.frame(
##     row = rep(well_rows, ncol(.df)),
##     col = rep(1:ncol(.df), each = length(well_rows)))
##
##   positive <- positive[complete.cases(positive), ]
##   indeterminate <- indeterminate[complete.cases(indeterminate), ]
##
##   ## set-style
##   if (nrow(positive) > 0) {
##     setCellStyle(wb, sheet = sheet_name, row = positive$row, col = positive$col,
##                  cellstyle = cs_positive)
##   }
##   if (nrow(indeterminate) > 0) {
##     setCellStyle(wb, sheet = sheet_name, row = indeterminate$row,
##                col = indeterminate$col,
##                cellstyle = cs_indeterminate)
##   }
##   if (nrow(to_repeat) > 0) {
##     setCellStyle(wb, sheet = sheet_name, row = to_repeat$row,
##                  col = to_repeat$col,
##                  cellstyle = cs_to_repeat)
##   }
##   setCellStyle(wb, sheet = sheet_name, row = 1, col = seq_along(names(.df)),
##                cellstyle = cs_header)
##
##   ## setCellStyle(wb, sheet = sheet_name, row = positive$row,
##   ##              col = positive$col,
##   ##              cellstyle = cs_border_positive)
##   ## setCellStyle(wb, sheet = sheet_name, row = indeterminate$row,
##   ##              col = indeterminate$col,
##   ##              cellstyle = cs_border_indeterminate)
##   setCellStyle(wb, sheet = sheet_name, row = 1, col = seq_along(names(.df)),
##                cellstyle = cs_border_header)
##   ## save-wb
##   saveWorkbook(wb)
##   excel_file
## }

#' Returns dataframe with multiple combined results from antigen results
#'
#' The following results set are possible:
#'  antigen 1 P and antigen 2 P, combined is P
#'  antigen 1 P and antigen 2 N, combined is I
#'  antigen 1 N and antigen 2 P, combined in I
#'  antigen 1 N and antigen 2 N, combined in N
#'
#' @param conn database connection object
#' @param all_r_mfi_df antigen results
#' @import stringi
#' @export
get_all_combined <- function(conn, all_r_mfi_df) {
  all_combined_df <- data.frame()
  for (file in as.character(unique(all_r_mfi_df$file))) {
    r_mfi_df <- all_r_mfi_df[all_r_mfi_df$file == file, ]
    combined_df <- get_combined(conn, r_mfi_df, file)
    all_combined_df <- rbind(all_combined_df, combined_df)
  }
  all_combined_df
}
#' Form an Excel workbook and worksheet similar in format to those being
#' made by Luis Giavedoni's laboratory.
#'
#' The Excel file ``Report plate 62 08-15-2014.xlsx'' was used as a guide.
#' @param file character vector of length one with name of file to create
#' @param w_raw_mfi_df dataframe with raw results from Luminex machine
#' @param w_mfi_df dataframe with background substracted from raw results
#' @param w_n_mfi_df dataframe with normalized values where divisors were
#' used on background subtracted results
#' @param w_r_mfi_df dataframe where each antigen result is called as N, I, or
#' P based on is values relative to a fraction of the low control value.
#' @param w_combined_df dataframe with the antigen calls combined into agent
#' (virus) calls. (See get_combined() for algorithm).
#' @param low_positive_controls_df dataframe with \code{sample}, \code{name},
#' \code{wells}, and \code{val} columns that identify the well with low
#' positive control values (<5000 for Human or Goat IgG).
#'
#' @import rmsutilityr
#' @export
make_excel_wkbk <- function(file, w_raw_mfi_df, w_mfi_df, w_n_mfi_df,
                            w_r_mfi_df, w_combined_df,
                            low_positive_controls_df) {
  w_raw_mfi_df$file <- basename(w_raw_mfi_df$file)
  w_mfi_df$file <- basename(w_mfi_df$file)
  w_n_mfi_df$file <- basename(w_n_mfi_df$file)
  w_r_mfi_df$file <- basename(w_r_mfi_df$file)
  w_combined_df$file_name <- basename(w_combined_df$file_name)
  if (nrow(low_positive_controls_df) > 0) {
    df_list <- list(
      w_raw_mfi_df,
      w_mfi_df,
      w_n_mfi_df,
      w_r_mfi_df,
      w_combined_df,
      low_positive_controls_df)
    sheetnames <- c(
      "raw_mfi",
      "minus_background",
      "normalized",
      "antigen_called",
      "final_result",
      "low_positive_controls")
  } else {
    df_list <- list(
      w_raw_mfi_df,
      w_mfi_df,
      w_n_mfi_df,
      w_r_mfi_df,
      w_combined_df)
    sheetnames <- c(
      "raw_mfi",
      "minus_background",
      "normalized",
      "antigen_called",
      "final_result")
  }

  create_wkbk(file, df_list, sheetnames)
  ## code disabled to allow temporary use under Java 12 environment, which has a
  ## bug preventing use of XLConnect
  # sheets_index <- c(3, 4, 5)
  # for (i in seq_along(sheets_index)) {
  #   df_index <- c(4, 4, 5) # normalized sheet gets same format as
  #                                 # antigen sheet
  #   format_luminex_results(file, df_list[[df_index[i]]],
  #                          sheetnames[sheets_index[i]],
  #                          low_positive_controls_df)
  # }
  file
}
#' Returns a character vector of 'Y' if the sample indicates it is a
#' repeat else 'N'
#'
#' @param sample character vector with one or more sample Ids
#' @import stringi
#' @export
get_repeated_from_sample <- function(sample) {
  ifelse(toupper(stri_sub(sample, -2)) == "-R", "Y", "N")
}
#' Returns yyyymmdd in with optional separator between date units if
#' string looks like a date.
#'
#' @param dates character vector with possible dates
#' @param sep character string to be used to separate date units.
#' @import stringi
#' @export
get_yyyymmdd_from_possible_dates <- function(dates, sep = "-") {
  dates <- sapply(dates, function(.date) {
    #cat(stri_c(".date: ", .date, "\n"))
    if (is.na(.date)) {
      NA
    } else {
      if (stri_length(.date) >= 8 | stri_detect_regex(.date, "[-/]")) {
        if (stri_detect_regex(.date, "[-/]")) {
          if (stri_length(stri_split_regex(.date, "[-/]")[[1]][1]) == 4) {
            .date
          } else {
            mdy_to_yyyymmdd(.date, sep = sep)
          }
        } else if (stri_length(.date) < 8) {
          NA
        } else {
          stri_c(stri_sub(.date, 1, 4),
                 sep,
                 stri_sub(.date, 5, 6),
                 sep,
                 stri_sub(.date, 7, 8))
        }
      } else {
        .date
      }
    }
  })
  names(dates) <- NULL
  dates
}

#' Returns samples with "Bn" and "-R" trimmed off of each end
#'
#' @param samples character vector of sample descriptors
#' @import stringi
#' @export
remove_bn_and_dash_R <- function(samples) {
  samples <- stri_trim_both(sapply(stri_trim_both(samples), function(sample) {
    if (!is.na(sample)) {
      if (stri_sub(sample, -2) == "-R")
        sample <- stri_sub(sample, 1, -3)
      if (toupper(stri_sub(sample, 1, 2)) == "BN")
        sample <- stri_sub(sample, 3)
    }
    sample}))
  names(samples) <- NULL
  samples
}
#' Returns a list of length 2 with possible$ids and possible$dates
#'
#' @param samples character vector of sample descriptors
#' @import stringi
#' @export
get_possible_ids_dates <- function(samples) {
  samples <- remove_bn_and_dash_R(samples)
  ids <- sapply(stri_split_regex(samples, "[ -]"), function(x) {
    x[[1]][1]})
  ids[!stri_detect_regex(ids, "[0-9]")] <- NA
  dates <- sapply(stri_split_regex(samples, "[ -]"), function(x) {
    len <- length(x)
    if (len > 1) {
      x[length(x)]
    } else {
      NA
    }
  })
  dates[!stri_detect_regex(dates, "[0-9]")] <- NA
  dates <- get_yyyymmdd_from_possible_dates(dates)
  ids <- get_yyyymmdd_from_possible_dates(ids)
  list(ids = ids, dates = dates)
}

#' Returns a list with two character vectors containing the items in the
#' vector dates that are not Ids and the items in ids that are not Ids.
#'
#' @param conn database connection object
#' @param ids character vector of possible Ids (or dates)
#' @param dates character vector of possible Ids (or dates)
#' @import animalr
#' @import RODBC
#' @import stringi
get_bad_date_ids_and_id_ids <- function(conn, ids, dates) {
  bad_ids <-
    stri_trim_both(suppressWarnings(is_bad_animal_id(conn, ids[!is.na(ids)])))
  bad_date_ids <-
    stri_trim_both(suppressWarnings(is_bad_animal_id(conn,
                                                     dates[!is.na(dates)])))
  list(bad_ids = bad_ids, bad_date_ids = bad_date_ids)
}
#' Returns the first integer value from a sample column if it exist else NA.
#'
#' @param conn database connection object
#' @param samples character vector with one or more sample Ids
#' @import stringi
#' @export
get_id_from_sample <- function(conn, samples) {
  possible <- get_possible_ids_dates(samples)
  bad <- get_bad_date_ids_and_id_ids(conn, possible$ids, possible$dates)
  date_is_id <- !possible$dates %in% bad$bad_date_ids
  id_is_id <- !possible$ids %in% bad$bad_ids
  if (any(date_is_id & id_is_id & !is.na(possible$dates) &
          !is.na(possible$ids)))
    stop(stri_c("samples has two valid animal Ids: ",
                get_and_or_list(samples[date_is_id & id_is_id])))
  possible$ids[!is.na(possible$dates) & date_is_id & !id_is_id] <-
    possible$dates[!is.na(possible$dates) & date_is_id]
  possible$ids[possible$ids %in% bad$bad_ids] <- NA
  possible$ids
}
#' Returns the date from a sample column if it exist else NA.
#'
#' @param conn database connection object
#' @param samples character vector with one or more sample dates
#' @import stringi
#' @export
get_date_from_sample <- function(conn, samples) {
  possible <- get_possible_ids_dates(samples)
  bad <- get_bad_date_ids_and_id_ids(conn, possible$ids, possible$dates)
  date_not_id <- possible$dates %in% bad$bad_date_ids
  id_not_id <- possible$ids %in% bad$bad_ids
  if (any(date_not_id & id_not_id & !is.na(possible$dates) &
          !is.na(possible$ids) &
          stri_length(possible$dates) > 6 & stri_length(possible$ids) > 6))
    stop(stri_c("samples may have multiple valid dates: ",
                get_and_or_list(samples[date_not_id & id_not_id])))
  possible$dates[id_not_id] <- possible$ids[id_not_id]
  possible$dates[stri_length(possible$dates) < 8] <- NA
  if (any(is.na(possible$dates)) &
      any(!stri_detect_fixed(samples[is.na(possible$dates)], "CNT"))) {
    warning(stri_c(
      "The following sample labels do not have recognizable dates: ",
      get_and_or_list(samples[is.na(possible$dates) &
                                !stri_detect_fixed(samples, "CNT")])))
  }
  possible$dates
}
#' Returns character vector with the raw data (raw mfi) table
#'
#' @param content character vector with lines from the Excel file worksheet.
#' @export
get_raw_mfi_tables <- function(content) {
  possible_id_headers <- c("groupid", "animalid", "groupname")

  for (i in 1:nrow(content)) {
    if (tolower(content[[i, 1]]) %in% possible_id_headers)
      break
  }
  start <- i - 1
  for (i in start:nrow(content)) {
    if (i > 4 & tolower(content[[i, 1]]) %in% possible_id_headers)
      break
  }
  end <- i - 2
  data.frame(content[start:end, ], stringsAsFactors = FALSE)
}
#' Returns character vector with the result tables
#'
#' @param content character vector with lines from the Excel file worksheet.
#' @export
get_result_tables <- function(content) {
  possible_id_headers <- c("groupid", "animalid")

  for (i in 20:nrow(content)) {
    if (tolower(content[[i, 1]]) %in% possible_id_headers)
      break
  }
  start <- i
  for (i in start:nrow(content)) {
    if (is.na(content[[i, 1]]))
      break
  }
  end <- i - 1
  data.frame(content[start:end, ], stringsAsFactors = FALSE)
}

#' Get bead call multipliers from results section of Excel SPF Report file.
#'
#' Takes a dataframe with the results section of the data in an Excel
#' SPF Report file, reads in the cuttoff values from the sheet, and creates
#' a dataframe with the bead call results to use as multipliers.
#'
#' @return dataframe with bead call results from the results section of the
#' Excel file.
#'
#' @param results character vector with line from the Excel file worksheet
#' having only the result table records
#' @import stringi
#' @export
get_bead_df <- function(results) {
  col_names <- as.character(results[1, ])
  col_names <- col_names[1:(which(col_names %in% c(NA, "NA"))[1] - 1)]
  if (any(as.character(results[ , 1]) %in% c(NA, "NA"))) {
    n_rows <- which(as.character(results[ , 1]) %in% c(NA, "NA"))[1] - 1
  } else {
    n_rows <- length(as.character(results[ , 1]))
  }
  bead_df <- results[2:n_rows, 1:length(col_names)]
  col_names[stri_detect_fixed(tolower(col_names), "animal")] <- "Animal.ID"
  names(bead_df) <- col_names
  bead_df
}
#' Returns dataframe with true animal Ids (Id) and date strings (Date) from
#' Animal.ID column.
#'
#' @param animal_id_col character vector containing the composite descriptor
#' that has the animal Id and sample date. I may also have an 'R' indicating
#' that the sample is a repeated sample.
#' @import stringi
#' @export
get_id_date_repeated <- function(animal_id_col) {
  repeated <- stri_detect_fixed(toupper(animal_id_col), "R")
  id_dates <- stri_trim_both(unlist(stri_split_fixed(animal_id_col, "-")))
  id_dates <- id_dates[toupper(id_dates) != "R"]
  is_id_col <- stri_length(id_dates) <= 6
  dates <- id_dates[!is_id_col]
  long_dates <- dates[stri_detect_fixed(dates, "/")]
  dates[stri_detect_fixed(dates, "/")] <-
    stri_c(stri_sub(long_dates, 7, 10),
           stri_sub(long_dates, 1, 2),
           stri_sub(long_dates, 4, 5))
  data.frame(Id = id_dates[is_id_col], Date = dates,
             Repeated = ifelse(repeated, "Y", "N"), stringsAsFactors = FALSE)
}
#' Returns a dataframe with the Id, Date, and Repeated column added, which
#' are derived from the Animal.ID column.
#'
#' @param df dataframe having at least the Animal.ID column
#' @return a dataframe with the Id, Date, and Repeated column added
#' @export
add_id_date_repeated <- function(df) {
  data.frame(get_id_date_repeated(df$Animal.ID), df, stringsAsFactors = FALSE)
}
#' Returns dataframe with summary results from the results section of the Excel
#' file.
#'
#' @param results character vector with line from the Excel file worksheet
#' having only the result table records
#' @export
get_summary_df <- function(results) {
  col_names <- as.character(results[1, ])
  len <- length(col_names)
  col_names <- col_names[(which(col_names %in% NA)[1] + 1):length(col_names)]
  col_names <-
    col_names[(which(!col_names %in% c(NA, "NA"))[1]):length(col_names)]
  offset <- len - length(col_names)
  col_names <- col_names[1:(which(col_names %in% c(NA, "NA"))[1] - 1)]
  col_names <- remove_string(col_names, "SIV mac")
  if (any(as.character(results[ , 1]) %in% NA)) {
    n_rows <- which(as.character(results[ , 1]) %in% c(NA, "NA"))[1] - 1
  } else {
    n_rows <- length(as.character(results[ , 1]))
  }
  summary_df <- results[2:n_rows, (1:length(col_names)) + offset]
  names(summary_df) <- col_names
  summary_df
}
#' Returns a dataframe with all summary results combined.
#'
#' Columns missing in some results set are added with NA values.
#'
#' @param path file path to directory containing files to be read.
#' @param col_names character vector of column names in the order they are to
#' appear.
#' @import stringi
#' @export
get_all_summary_df <- function(path = "../inst/extdata/",
                               col_names = c("Filename", "Animal.ID", "STLV",
                                             "SRV", "BV", "SIV", "Measles")) {
  files <- get_usable_files(path = path)
  n_cols <- length(col_names)
  all_summary_df <- data.frame(rbind(1:n_cols))
  names(all_summary_df) <- col_names

  for (file in files) {
    content <- read_excel(stri_c(path, "/", file))
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    results <- get_result_tables(content)
    summary_df <- get_summary_df(results)
    summary_df <- data.frame(Filename = rep(file, nrow(summary_df)), summary_df,
                             stringsAsFactors = FALSE)
    tmp_cols <- names(summary_df)
    missing_cols <- col_names[!col_names %in% tmp_cols]
    for (col in missing_cols) {
      summary_df <- cbind(summary_df, col = rep(NA, nrow(summary_df)))
    }
    names(summary_df) <- c(tmp_cols, missing_cols)
    summary_df <- summary_df[ , col_names] # place columns in correct order
    all_summary_df <- rbind(all_summary_df, summary_df)
  }
  all_summary_df[-1, ] # remove dummy row
}
#' Returns a dataframe with all bead or all summary results combined
#' depending on which function and column names are passed in.
#'
#' Columns missing in some results set are added with NA values.
#'
#' @param path file path to directory containing files to be read.
#' @param .fun the function object to be used to collect combined results.
#' @param col_names character vector of column names in the order they are to
#' appear.
#' @import stringi
#' @export
get_all_df <- function(path = "../inst/extdata/", .fun = get_summary_df,
                       col_names = get_col_names("syntactic_summary")) {
  files <- get_usable_files(path = path)
  n_cols <- length(col_names)
  all_df <- data.frame(rbind(1:n_cols))
  names(all_df) <- col_names

  for (file in files) {
    content <- read_excel(stri_c(path, "/", file))
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    results <- get_result_tables(content)
    part_df <- .fun(results)
    part_df <- data.frame(Filename = rep(file, nrow(part_df)), part_df,
                          stringsAsFactors = FALSE)
    tmp_cols <- names(part_df)
    missing_cols <- col_names[!col_names %in% tmp_cols]
    for (col in missing_cols) {
      part_df <- cbind(part_df, col = rep(NA, nrow(part_df)))
    }
    names(part_df) <- c(tmp_cols, missing_cols)
    part_df <- part_df[ , col_names] # place columns in correct order
    all_df <- rbind(all_df, part_df)
  }
  all_df[-1, ] # remove dummy row
}
#' Returns a dataframe with all headers found in the bead results as seen by
#' get_bead_df()
#'
#' @param path file path to directory containing files to be read.
#' @param files names of files to be read
#' @import readxl
#' @import stringi
#' @export
get_bead_headers <- function(path, files) {
  max_len <- 0
  headers <- list(length(files))

  for (i in seq_along(files)) {
    content <- read_excel(stri_c(path, "/", files[i]))
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    results <- get_result_tables(content)
    bead_df <- get_bead_df(results = results)
    len <- length(bead_df)
    headers[i] <- list(c(files[i], names(bead_df)))
    if (len > max_len)
      max_len <- len
  }
  max_len <- max_len + 1 # room for filename
  new_headers <- list(length(files))
  for (i in seq_along(headers)) {
    header <- headers[[i]]
    header <- c(header, rep(NA, max_len - length(header)))
    new_headers[i] <- list(header)
  }
  headers <- new_headers
  header_df <- rbind(1:max_len)
  names(header_df) <- stri_c("col_", 1:max_len)
  for (i in seq_along(headers)) {
    header_df <- rbind(header_df, headers[[i]])
  }
  header_df <- data.frame(header_df, stringsAsFactors = FALSE)
  names(header_df) <- c("Filename", stri_c("Col_", 1:(max_len - 1)))
  header_df[-1, ]
}
#' Returns the list of pathology report files with an extension of ".pdf"
#' found in folders indicated by the years between start_year and end_year
#' inclusive.
#'
#' @param start_year integer value indicating the first calendar year as used
#' in the name of the folders containing reports from the years indicated.
#' @param end_year integer value indicating the last calendar year as used in
#' the name of the folders containing reports from the years indicated.
get_all_files <- function(start_year, end_year) {
  files <- c()
  for (year in stri_c(seq(start_year, end_year))) {
    path <-  stri_c("/Volumes/Pathology/Path\ Reports/", year, "\ Reports/")
    #path <- stri_c("/Volumes/Path Reports/", year, "\ Reports")
    #path <- stri_c("../data/", year, " Reports/")
    files <- c(files, list.files(path, pattern = ".pdf", full.names = TRUE))
  }
  files
}
#' Returns a list of lists of length two containing $content and $meta lists
#' derived from each PDL report.
#'
#' The returned list contains one list of length 2 for each PDL report.
#' The 2 elements within each list are the content list, which has one element
#' for each line within the report and the meta list, which has 7 elements.
#' See get_pdl_meta_data_from_content().
#'
#' @param base_directory character vector with path for directory in which the
#' pdl_report_directory resides.
#' @param pdl_report_directory character vector with name of directory in which
#' the PDL reports reside.
#' @param start_year integer value indicating the first calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param end_year integer value indicating the last calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param excluded_files character vector of files to ignore.
#' @import tm
#' @export
get_pdl_reports <- function(base_directory = "../inst/extdata/",
                            pdl_report_directory = "PDL/",
                            start_year, end_year,
                            excluded_files = character(0)) {
  files <- c()
  for (year in stri_c(seq(start_year, end_year))) {
    path <- stri_c(base_directory, pdl_report_directory, year)
    files <- c(files, list.files(path, recursive = TRUE, pattern = ".pdf",
                                 full.names = TRUE))
  }
  #total_number_of_files <- length(files)
  for (excluded_file in excluded_files) {
    files <- files[!(stri_detect_fixed(files, excluded_file))]
  }

  pdl_reports <- list(length(files))
  for (i in seq_along(files)) {
    uri <- sprintf(stri_c("file://", "%s"),
                   files[i])
    if (all(file.exists(Sys.which(c("pdftotext",
                                   "pdfinfo"))))) {
      pdl_df <- readPDF(control = list(text = "-layout"))(
        elem = list(uri = uri), language = "en", id = "id1")
    }
    pdl_reports[i] <- list(pdl_df)
  }
  pdl_reports
}
#' Returns empty pdl_df
#'
#' @export
get_empty_pdl_df <- function() {
  data.frame(file_name = character(0),
             order_pk = character(0),
             print_date_tm = character(0),
             bill_po = integer(0),
             request_num = character(0),
             report_contact = character(0),
             received_date  = character(0),
             report_date = character(0),
             order_comment = character(0),
             sample = integer(0),
             pdl_animal_id = character(0),
             snprc_id = character(0),
             pooled = character(0),
             pdl_species = character(0),
             sample_type = character(0),
             sample_date = character(0),
             comment = character(0),
             test_type = character(0),
             procedure_id = integer(0),
             procedure_name = character(0),
             test_id = integer(0),
             test_name = character(0),
             results = character(0),
             test_comment = character(0),
             stringsAsFactors = FALSE)
}
#' Returns report date from a pdl_report object
#'
#' @param pdl_report list containing $content and $meta that make up the
#' complete data from PDF PDL report file.
#' @export
get_pdl_report_date <- function(pdl_report) {
  pdl_report$meta$datetimestamp
}
#' Returns specified value from a pdl_report$content object
#'
#' @param pdl_report list containing $content and $meta that make up the
#' complete data from PDF PDL report file.
#' @param meta_data_label character vector of string to search for that
#' is immediately followed by the meta data being sought
#' @param len maximum length of the meta data being retrieved.
#' @import stringi
#' @export
get_pdl_meta_data_from_content <- function(pdl_report, meta_data_label, len) {
  content <- pdl_report$content
  meta_data <- ""
  for (line in content) {
    if (stri_detect_fixed(line, meta_data_label)) {
      from <- stri_locate_first_fixed(line, pattern = meta_data_label)[2] + 1
      meta_data <- stri_trim_both(stri_sub(line, from = from, length = len))
      break
    }
  }
  meta_data
}

#' Returns report file name from a pdl_report object
#'
#' @param pdl_report list containing $content and $meta that make up the
#' complete data from PDF PDL report file.
#' @export
get_pdl_report_file_name <- function(pdl_report) {
  pdl_report$meta$id
}

#' Returns content for PDL reports without introductory material and end of
#' page material
#'
#' @param content copy of pdl_report$content taken from PDF PDL report
#' @export
remove_pdl_report_headers <- function(content) {
  new <- character(length(content))
  j <- 1
  for (i in seq_along(content)) {
    line <- content[i]
    if (!stri_detect_fixed(line, "Sample #")) {
      next
    } else {
      break
    }
  }
  for (k in i:length(content)) {
    line <- content[k]
    if (stri_detect_fixed(line, "Pathogen") |
      stri_detect_fixed(line, "Report") |
      stri_detect_fixed(line, "\f") |
      stri_detect_fixed(line, "Page") |
      stri_detect_fixed(line, "Order") |
      stri_detect_fixed(line, "Bill")) {
      next
    } else {
      new[j] <- line
      j <- j + 1
    }
  }
  new[nchar(stri_trim_both(new)) > 0]
}
#' Returns TRUE if a line is an expected set of column labels
#'
#' @param line character vector of length 1.
#' @import stringi
is_column_labels <- function(line) {
  expected <- c("Sample", "#", "An", "Id", "Sp", "Type", "Date", "Comment",
                "Test", "Type", "Results", "Test", "Comment")
  observed <- stri_trim_both(stri_split_boundaries(line, simplify = TRUE)[1, ])
  all(is.element(expected, observed))
}
#' Returns content minus column labels. Fails with a call to stop()
#' if anticipated labels are not present
#'
#' @param content character vector that has lines of report with data.
#' @import stringi
#' @export
strip_column_labels <- function(content) {
  new <- character(length(content))
  i <- 0
  for (line in content) {
    if (is_column_labels(line)) {
      next
    } else {
      i <- i + 1
      new[i] <- line
    }
  }
  new[1:i]
}
#' Returns an integer vector with the the positions in the line that begin
#' each column label.
#'
#' @param content charcter vector containing results. First line should contain
#' column labels.
#' @import stringi
get_col_boundaries <- function(content) {
  line <- content[1]
  start_end <- stri_locate_all_words(line)[[1]]
  start <- start_end[ , 1][c(1, 2, 4, 5, 6, 7, 8, 10, 11)]
  # Header is shifted one character to left of data for Animal Id
  start[2] <- start[2] - 1
  # Header is shifted one character to left of data for results
  start[9] <- start[9] - 1
  end <- as.integer(c(start[2:length(start)] - 1, max(nchar(content))))
  col_boundaries <- data.frame(sample = c(start[1], end[1]),
                               pdl_animal_id = c(start[2], end[2]),
                               pdl_species = c(start[3], end[3]),
                               sample_type = c(start[4], end[4]),
                               sample_date = c(start[5], end[5]),
                               comment = c(start[6], end[6]),
                               test_type = c(start[7], end[7]),
                               results = c(start[8], end[8]),
                               test_comment = c(start[9], end[9]))
  col_boundaries
}
#' Returns character vector of length 1 made up of text found in two character
#' vector elements as subsetted according to start and end locations.
#'
#' @param line_1 first line of text to subset
#' @param line_2 second line of text to subset
#' @param start_end column boundaries to include
#' @import stringi
#' @export
combine_lines <- function(line_1, line_2, start_end) {
  stri_c(stri_trim_both(stri_sub(line_1, start_end[1], start_end[2])),
         stri_trim_both(stri_sub(line_2, start_end[1], start_end[2])))
}
#' Returns "Y" if a sample is pooled and "N" if it is an individual sample.
#'
#' @param pdl_animal_id Id used by PDL to indicate the pooled sample or the
#' individual animal sample.
#' @import stringi
#' @export
pooled_Y_or_N <- function(pdl_animal_id) {
  if (toupper(stri_sub(pdl_animal_id, 1, 1)) == "V") {
    pooled <- "Y"
  } else {
    pooled <- "N"
  }
  pooled
}
#' Returns an SNPRC animal Id of the pdl_animal_id represents an individual
#' animal
#'
#' @param pdl_animal_id Id used by PDL to indicate the pooled sample or the
#' individual animal sample.
#' @import stringi
#' @export
get_snprc_id <- function(pdl_animal_id) {
  if (stri_detect_fixed(pdl_animal_id, pattern = "-")) {
    snprc_id <- stri_split_fixed(pdl_animal_id,
                                                pattern = "-")[[1]][2]
  } else if (stri_sub(pdl_animal_id, 1, 1) == "V") {
    snprc_id <- ""
  } else {
    snprc_id <- pdl_animal_id
  }
  if (toupper(stri_sub(snprc_id, 1, 2)) == "BN") {
    snprc_id <- stri_sub(snprc_id, 3)
  }
  snprc_id
}
#' Returns a character vector of length 2 with the result and comment
#'
#' @param line_1 first line of data
#' @param line_2 second line of data
#' @param col_boundaries locations of start and end column for each data type
#' @import stringi
#' @export
get_pdl_results_test_comment <- function(line_1, line_2, col_boundaries) {
  r_start_end <- col_boundaries$results
  results <- stri_split_charclass(
    stri_sub(line_1, r_start_end[1]), "\\p{WHITE_SPACE}")[[1]][1]
  start_end <- col_boundaries$test_comment
  start_end[1] <- r_start_end[1] + stri_length(results)
  test_comment <-
    stri_c(stri_trim_both(stri_sub(line_1, start_end[1], start_end[2])),
           stri_trim_both(stri_sub(line_2, start_end[1], start_end[2])))
  list(results = results, test_comment = test_comment)
}

#' Returns a one record dataframe with the data from one assay record.
#'
#' @param file_name name of file according to $meta$id
#' @param order_pk order primary key
#' @param print_date_tm print date of report
#' @param bill_po bill number
#' @param request_num request number
#' @param report_contact report contact
#' @param received_date date samples were received
#' @param report_date date the report was generated according to
#' $meta$datetimestamp
#' @param order_comment comment provided by SNPRC about the order
#' @param line_1 first line of data
#' @param line_2 second line of data
#' @param col_boundaries locations of start and end column for each data type
#' @export
get_one_record_df <- function(file_name, order_pk,
                              print_date_tm, bill_po, request_num,
                              report_contact, received_date,
                              report_date, order_comment, line_1,
                              line_2, col_boundaries) {
  #cat(stri_c(line_1, "\n"))
  results_test_comment <-
    get_pdl_results_test_comment(line_1, line_2, col_boundaries)
  data.frame(file_name = file_name,
             order_pk = order_pk,
             print_date_tm = print_date_tm,
             bill_po = bill_po,
             request_num = request_num,
             report_contact = report_contact,
             received_date = received_date,
             report_date = report_date,
             order_comment = order_comment,
             sample = combine_lines(line_1, line_2,
                                    col_boundaries$sample),
             pdl_animal_id = combine_lines(line_1, line_2,
                                col_boundaries$pdl_animal_id),
             snprc_id = get_snprc_id(combine_lines(
               line_1, line_2, col_boundaries$pdl_animal_id)),
             pooled = pooled_Y_or_N(combine_lines(
               line_1, line_2, col_boundaries$pdl_animal_id)),
             pdl_species = combine_lines(line_1, line_2,
                                     col_boundaries$pdl_species),
             sample_type = combine_lines(line_1, line_2,
                                         col_boundaries$sample_type),
             sample_date = combine_lines(line_1, line_2,
                                         col_boundaries$sample_date),
             comment = combine_lines(line_1, line_2,
                                     col_boundaries$comment),
             test_type = combine_lines(line_1, line_2,
                                       col_boundaries$test_type),
             results = results_test_comment$results,
             test_comment = results_test_comment$test_comment,
             stringsAsFactors = FALSE)

}
#' Returns character vector representing the contents of the pdl_report$content
#' with the missing second lines of data replaced.
#'
#' @param content character vector representing the contents of the
#'  pdl_report$content with the possible missing second lines of data
#' @import stringi
#' @export
insert_missing_lines <- function(content) {
  new <- character(length(content))
  j <- 0
  last_line <- "second"
  second_line <- "          "
  for (line in content) {
    len <- nchar(stri_trim_both(stri_sub(line, from = 1, to = 10)))
    if (len == 0) {
      j <- j + 1
      new[j] <- line
      last_line <- "second"
    } else if (last_line == "first" & len > 0) {
      #last_line <- "second"
      j <- j + 1
      new[j] <- second_line
      j <- j + 1
      new[j] <- line
    } else if (last_line == "second" & len > 0) {
      last_line <- "first"
      j <- j + 1
      new[j] <- line
    }
  }
  if (last_line == "first") {
    j <- j + 1
    new[j] <- second_line
  }
  new[1:j]
}
#' Returns a character vector of SqlMed procedure_names given a character
#' vector of Luminex_assay.
#'
#' @param luminex_species character vector of agent; case sensitive
#' @param luminex_repeated character vector of whether the assay was
#' repeated for the animal
#' @export
luminex_to_sqlmed_procedure_name <-
  function(luminex_species, luminex_repeated) {
  procedure_name <- character(length(luminex_species))
  procedure_name[luminex_species == "MACAQUE" |
                   luminex_species == "CYNO"] <- "RHESUS SPF SCREEN"
  procedure_name[luminex_species == "PAPIO"] <- "BABOON SPF SCREEN"
  procedure_name
}
#' Returns a numeric vector of SqlMed procedure_id given a character
#' vector of luminex_species and luminex_repeatedy.
#'
#'Warning luminex does not provide species designations reliably.
#'This will have to come from post processing.
#'
#' @param luminex_species character vector "MACAQUE" or "PAPIO"
#' @param luminex_repeated characer vector with "Y" or "N" depending on
#' whether the sample ended in "-R" or not.
#' @export
luminex_to_sqlmed_procedure_id <-
  function(luminex_species, luminex_repeated) {
  procedure_id <- integer(length(luminex_species))
  procedure_id[luminex_species == "MACAQUE" |
                 luminex_species == "CYNO"] <- 10637
  procedure_id[luminex_species == "PAPIO"] <- 10636
  procedure_id[luminex_repeated == "Y" & luminex_species == "MACAQUE"] <- 10641
  procedure_id
}
#' Returns a character vector of SqlMed test_name given a character
#' vector of luminex_agent.
#'
#' @param luminex_agent character vector of pdl_test; case sensitive
#' @export
luminex_agent_to_sqlmed_test_name <- function(luminex_agent) {
  as.character(
    c(
      "SIV" = "SIV AB",
      "SRV" = "SRV AB",
      "BV" = "HERPES B VIRUS",
      "STLV" = "STLV-1 AB",
      "Measles" = "MEASLES",
      "T_cruzi" = "T. CRUZI AB")[luminex_agent])
}
#' Returns a numeric vector of SqlMed test_id given a character
#' vector of luminex_agent.
#'
#' @param luminex_agent character vector of pdl_test; case sensitive
#' @export
luminex_agent_to_sqlmed_test_id <- function(luminex_agent) {
  as.integer(
    c("Measles" = 786,
      "SIV" = 788,
      "SRV" = 787,
      "BV" = 785,
      "STLV" = 789,
      "T_cruzi" = 850)[luminex_agent])
}
#' Returns a character vector of SqlMed procedure_names given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_procedure_name <- function(pdl_assay) {
  procedure_name <- rep("VIRAL ANTIBODY SCREEN", length(pdl_assay))
  procedure_name[pdl_assay == "STLV PCR"] <- "STLV-1 BY PCR"
  procedure_name
}
#' Returns a character vector of SqlMed procedure_id given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_procedure_id <- function(pdl_assay) {
  procedure_id <- ifelse(pdl_assay == "STLV PCR", 10620, 10616)
  procedure_id
}
#' Returns a character vector of SqlMed test_name given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_test_name <- function(pdl_assay) {
  as.character(c(
    "SIV AB" = "SIV AB",
    "SIV MIA" = "SIV AB",
    "SIV PCR" = "SIV PCR",
    "SIV WB" = "SIV WB",
    "HERPES B MIA" = "HERPES B VIRUS",
    "HVP2 MIA" = "HVP-2",
    "MEASLES MIA" = "MEASLES",
    "SRV MIA" = "SRV AB",
    "SRV PCR" = "SRV PCR",
    "SRV1 WB" = "SRV1 WB",
    "SRV2 WB" = "SRV2 WB", # this is made up
    "STLV MIA" = "STLV-1 AB",
    "STLV AB" = "STLV-1 AB",
    "STLV PCR" = "STLV-1 BY PCR",
    "STLV WB" = "STLV WB")[pdl_assay])
}
#' Returns a character vector of SqlMed test_id given a character
#' vector of pdl_assay.
#'
#' @param pdl_assay character vector of pdl_test; case sensitive
#' @export
pdl_assay_to_sqlmed_test_id <- function(pdl_assay) {
  as.integer(c(
    "SIV AB" = 788,
    "SIV MIA" = 788,
    "SIV PCR" = 852,
    "SIV WB" = 948,
    "HERPES B MIA" = 785,
    "HVP2 MIA" = 917,
    "MEASLES MIA" = 786,
    "SRV MIA" = 787,
    "SRV AB" = 787,
    "SRV PCR" = 875,
    "SRV1 WB" = 943,
    "SRV2 WB" = 944, # this is made up
    "STLV MIA" = 793,
    "STLV AB" = 793,
    "STLV PCR" = 898,
    "STLV WB" = 947)[pdl_assay])
}
#' Returns df dataframe with the SqlMed procedure_name, procedure_id,
#' test_id, and test_name values added to the df.
#'
#' @param df dataframe with the data from the PDL reports.
#' @param lab character vector of length one that indicates the laboratory
#' @export
add_sqlmed_codes <- function(df, lab) {

  if (toupper(lab) == "PDL") {
    df$procedure_id <- pdl_assay_to_sqlmed_procedure_id(df$test_type)
    df$procedure_name <- pdl_assay_to_sqlmed_procedure_name(
      df$test_type)
    df$test_id <- pdl_assay_to_sqlmed_test_id(df$test_type)
    df$test_name <- pdl_assay_to_sqlmed_test_name(df$test_type)
  } else if (toupper(lab) == "GIAVEDONI") {
    df$procedure_id <- luminex_to_sqlmed_procedure_id(df$species, df$repeated)
    df$procedure_name <- luminex_to_sqlmed_procedure_name(df$species,
                                                          df$repeated)
    df$test_id <- luminex_agent_to_sqlmed_test_id(df$agent)
    df$test_name <- luminex_agent_to_sqlmed_test_name(df$agent)
  }
  df
}
#' Returns the corrected sample type given slightly malformed or incomplete
#' sample types.
#'
#' @param sample_type character vector of sample types to be translated.
#' @import stringi
#' @export
get_sample_type_trans <- function(sample_type) {
  sample_type[stri_detect_fixed(sample_type, "WHOLE")] <- "WHOLE BLOOD"
  sample_type
}
#' Returns dataframe with parsed content of pdl_report
#'
#' @param pdl_report list containing $content and $meta that make up the
#' complete data from PDF PDL report file.
#' @import stringi
#' @export
pdl_report_to_df <- function(pdl_report) {
  file_name <- get_pdl_report_file_name(pdl_report)
  order_pk <- get_pdl_meta_data_from_content(pdl_report, "Order Pk:", 14)
  print_date_tm  <- get_pdl_meta_data_from_content(pdl_report,
                                                   "Report printed on:", 28)
  bill_po <- get_pdl_meta_data_from_content(pdl_report, "Bill Po:", 10)
  request_num <- get_pdl_meta_data_from_content(pdl_report, "Req Num:", 10)
  report_contact <- get_pdl_meta_data_from_content(pdl_report, "Rpt Contact:",
                                                   20)
  received_date <- get_pdl_meta_data_from_content(pdl_report, "Recd Dt:", 11)
  report_date  <- get_pdl_meta_data_from_content(pdl_report, "Report Dt:", 11)
  order_comment <- get_pdl_meta_data_from_content(pdl_report, # to end of line
                                                  "Order Comment:", 1000)
  content <- remove_pdl_report_headers(pdl_report$content)
  col_boundaries <- get_col_boundaries(content)
  content <- strip_column_labels(content)
  # This is needed because the report clips the last line of the page off
  # if the report goes to the next page.
  content <- insert_missing_lines(content)
  len <- length(content)
  pdl_df <- get_empty_pdl_df()
  for (i in seq_along(content)[is_odd(seq_along(content))]) {
    line_1 <- content[i]
    if (len > i) {
      line_2 <- content[i + 1]
      pdl_df <-
        rbind(pdl_df, get_one_record_df(file_name, order_pk,
                                        print_date_tm, bill_po, request_num,
                                        report_contact, received_date,
                                        report_date, order_comment, line_1,
                                        line_2, col_boundaries))
    }
  }
  pdl_df <- add_sqlmed_codes(pdl_df, "PDL")
  pdl_df$sample_type <- get_sample_type_trans(pdl_df$sample_type)
  pdl_df
}
#' Returns dataframe with results from all report files in directory.
#'
#' @param base_directory path of base directory
#' base_directory = ".."
#' @param pdl_report_directory name of directory containing PDL reports
#' pdl_report_directory = "inst/extdata/"
#' @param start_year integer value indicating the first calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param end_year integer value indicating the last calendar year as used in
#' the name of the folders containing reports from the years indicated.
#' @param excluded_files character vector of files within the PDL directory
#' that are to be ignored.
#' excluded_files = character(0))
#' @export
pdl_reports_to_df <- function(base_directory, pdl_report_directory,
                              start_year, end_year,
                              excluded_files = character(0)) {
  pdl_reports <- get_pdl_reports(base_directory, pdl_report_directory,
                                 start_year, end_year,
                                 excluded_files)
  pdl_df <- get_empty_pdl_df()
  for (i in seq_along(pdl_reports)) {
    pdl_df <- rbind(pdl_df, pdl_report_to_df(pdl_reports[[i]]))
  }
  add_sqlmed_codes(pdl_df, "PDL")
}

#' Returns non-negative results from PDL reports results
#'
#' @param pdl_df dataframe with data from one or more PDF PDL report files.
#' @import stringi
#' @export
get_pdl_nonnegative_reports <- function(pdl_df) {
  pdl_df[toupper(stri_sub(pdl_df$results, 1, 1)) != "N", ]
}
#' Returns a list of samples that were expected but not
#' received.
#'

#' @param sample_df dataframe from sample file
#' @param file_name fully qualified file name of Excel file
#' @import rmsutilityr
#' @import stringi
#' @export
report_unexpected_samples <- function(sample_df, file_name) {
  if (!all(is.na(sample_df$snprc_id[sample_df$blood_received == "yes"]))) {
    unexpected_samples <-
      sample_df[sample_df$blood_received == "yes" &
                      sample_df$blood_expected == "no", ]
    if (nrow(unexpected_samples) > 0) {
      unexpected_samples$cage <- round(unexpected_samples$cage, 2)
      create_wkbk(file = "../reports/unexpected_samples_log.xlsx",
                  list(unexpected_samples), "unexpected samples")
      warning(stri_c("There are one or more blood samples that were not ",
                     "expected were received in a sample. They are listed in ",
                  "../reports/unexpected_samples_log.xlsx."))
    }
  }
}
#' Creates an Excel file with a list of samples that were expected but not
#' received.
#'
#'
#' This needs testing.
#'
#' @param sample_df dataframe from sample file
#' @import rmsutilityr
#' @import stringi
#' @export
report_missing_samples <- function(sample_df) {
  if (!all(is.na(sample_df$snprc_id[sample_df$blood_received == "no"]))) {
    missing_samples <-
      sample_df[sample_df$blood_received == "no" &
                      sample_df$blood_expected == "yes", ]
    if (nrow(missing_samples) > 0) {
      missing_samples$cage <- round(missing_samples$cage, 2)
      create_wkbk(file = "../reports/missing_samples_log.xlsx",
                  list(missing_samples), "missing samples")
      warning(stri_c("There are one or more blood ",
                  "samples that were expected were not received. ",
                  "They are listed in ",
                  "../reports/missing_samples_log.xlsx."))
    }

  }
}
#' Throws and error on bad input for the \code{expected_blood}
#' or \code{received_blood} column
#' that terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param expected character vector of values from the \code{expected_blood}
#' or \code{received_blood} column of a blood sample file.
#' @param file_name character vector of length one with the file name.
#' @param col_name character vector of length one with column name.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import rmsutilityr
#' @import stringi
#' @export
test_for_yes_no <- function(expected, file_name, col_name, run_props,
                            run_error) {
  if (any(!tolower(expected) %in% c("yes", "no"))) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             '", col_name, "' values that are not either 'yes' or
             'no'. They are on the following row(s): ",
             get_and_or_list(seq_along(expected)[
               !tolower(expected) %in% c("yes", "no")]),
             " with the following respective value(s): ",
             get_and_or_list(expected[
               !tolower(expected) %in% c("yes", "no")]), "."))
  }
}
#' Throws and error on one or more bad animal Ids.
#' It terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param snprc_id character vector of \code{snprc_id}
#' @param file_name character vector of length one with the file name.
#' @param conn database connection object to the animal database
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import animalr
#' @import rmsutilityr
#' @import stringi
#' @export
test_for_snprc_id <- function(snprc_id, file_name, conn, run_props, run_error) {
  not_primate <- !is_primate_id(conn, snprc_id)
  if (any(not_primate)) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             snprc_id values that are not primates.
             They are on the following row(s): ",
             get_and_or_list(seq_along(snprc_id)[not_primate]),
             " with the following respective value(s): ",
             get_and_or_list(snprc_id[not_primate]), "."))
  }
}
#' Throws and error on one or more bad cage numbers are provided.
#' It terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param cage numeric vector of \code{cage}
#' column of a blood sample file.
#' @param file_name character vector of length one with the file name.
#' @param conn database connection object to the animal database
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import animalr
#' @import rmsutilityr
#' @import stringi
#' @export
test_for_cage <- function(cage, file_name, conn, run_props, run_error) {
  not_cage <- !is_location(conn, cage)
  if (any(not_cage)) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             cage locations values that are not valid locations.
             They are on the following row(s): ",
             get_and_or_list(seq_along(cage)[not_cage]),
             " with the following respective value(s): ",
             get_and_or_list(cage[not_cage]), "."))
  }
}
#' Throws and error on one or more provided bleed dates occur on a date that
#' the animal was not alive. If the animal never existed at SNPRC, it was not
#' alive.
#' It terminates the script with an informative
#' message saying what was wrong otherwise it simply returns NULL.
#'
#' @return NULL if test finds no bad input.
#' @param snprc_id character vector of \code{snprc_id}
#' column of a blood sample file.
#' @param bleed_date character vector ofb  bv
#' @param file_name character vector of length one with the file name.
#' @param conn database connection object to the animal database
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import animalr
#' @import rmsutilityr
#' @import stringi
#' @export
test_for_bleed_date <- function(snprc_id, bleed_date, file_name, conn,
                                run_props, run_error) {
  id_date_df <- data.frame(id = snprc_id, sample_date = bleed_date)
  bad_date <- !is_animal_alive(conn, id_date_df)
  if (any(bad_date)) {
    triggerError(
      run_props, run_error,
      stri_c("The file, ", file_name, ", has one or more
             animal Id - bleed date combinations are not possible because
             the indicated animal was not alive at the SNPRC on the bleed
             date indicated.
             They are on the following row(s): ",
             get_and_or_list(seq_along(snprc_id)[bad_date]),
             " with the following respective value(s): ",
             get_and_or_list(stri_c(snprc_id[bad_date], " on date ",
                                    bleed_date[bad_date])), "."))
  }
}
#' Read blood or dna sample file
#'
#' @return dataframe with animal Ids, cage location, bleed date, whether not a
#' blood sample was expected, whether or not a sample was received, and the
#' Pool Id number if present for a single sample file.
#'
#' @param file_name fully qualified file name of Excel file
#' @param conn database connection object.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import readxl
#' @import stringi
#' @export
read_sample_file <- function(file_name, conn, run_props, run_error) {
  sample_df <- data.frame(file_name = character(0),
                                 snprc_id = character(0),
                                 cage = numeric(0),
                                 bleed_date = character(0),
                                 blood_expected = character(0),
                                 blood_received = character(0),
                                 pool_id = character(0))
  tmp_sample_df <- read_excel(file_name, col_names = TRUE)
  tmp_sample_df <- Filter(function(x)!all(is.na(x)), tmp_sample_df)
  sample_col_names <- names(sample_df)
  if (any("pool_id" %in% setdiff(sample_col_names, names(tmp_sample_df)))) {
    pooled_samples <- FALSE
  } else {
    pooled_samples <- TRUE
  }
  if (length(tmp_sample_df) == length(sample_col_names)) {
    tmp_sample_df <- tmp_sample_df[ , -1] # remove unused row number
  } else if (pooled_samples &
             !length(tmp_sample_df) == (length(sample_col_names) - 1)) {
    triggerError(stri_c("File: ", file_name,
                " does not have the correct number of columns. ",
                "It should have the following columns: ",
                get_and_or_list(sample_col_names[-1]), "."))
  } else if (!pooled_samples &
             !length(tmp_sample_df) == (length(sample_df) - 2)) {
    triggerError(
      stri_c("File: ", file_name,
             " does not have the correct number of columns. ",
             "It should have the following columns: ",
             get_and_or_list(sample_col_names[
               c(-1, -length(sample_col_names))]), "."))
  }
  if (!pooled_samples) {
    tmp_sample_df$pool_id <- NA
  }
  names(tmp_sample_df) <- sample_col_names[-1] # does not have file

  tmp_sample_df <- data.frame(tmp_sample_df)
  tmp_sample_df <-
    tmp_sample_df[!is.na(tmp_sample_df$snprc_id), ]
  tmp_sample_df$cage <- as.numeric(tmp_sample_df$cage)
  tmp_sample_df$bleed_date <-
    stri_datetime_format(
      stri_datetime_parse(tmp_sample_df$bleed_date, format = "uuuuMMdd"),
      format = "uuuu-MM-dd")
  tmp_sample_df$snprc_id <- blank_fill_ids(tmp_sample_df$snprc_id)

  test_for_yes_no(tmp_sample_df$blood_expected, file_name, "blood_expected",
                  run_props, run_error)
  test_for_yes_no(tmp_sample_df$blood_received, file_name, "blood_received",
                  run_props, run_error)
  test_for_snprc_id(tmp_sample_df$snprc_id, file_name, conn, run_props,
                    run_error)
  test_for_cage(tmp_sample_df$cage, file_name, conn, run_props, run_error)
  test_for_bleed_date(tmp_sample_df$snprc_id, tmp_sample_df$bleed_date,
                      file_name, conn, run_props, run_error)
  tmp_sample_df$blood_expected[
    tolower(tmp_sample_df$blood_expected) == "yes"] <- "Y"
  tmp_sample_df$blood_expected[
    tolower(tmp_sample_df$blood_expected) == "no"] <- "N"
  tmp_sample_df$blood_received[
    tolower(tmp_sample_df$blood_received) == "yes"] <- "Y"
  tmp_sample_df$blood_received[
    tolower(tmp_sample_df$blood_received) == "no"] <- "N"
  tmp_sample_df$pooled <- ifelse(stri_detect_regex(tmp_sample_df$pool_id, "^V"),
                                 "Y", "N")
  tmp_sample_df$pooled[is.na(tmp_sample_df$pool_id)] <- "N"
  tmp_sample_df
}
#' Returns dataframe with animal Ids, cage location, bleed date, whether not a
#' blood sample was expected, whether or not a sample was received, and the
#' Pool Id number if present.
#'
#' @param all_files charcter vector of all the files to process.
#' @param conn database connection object.
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import stringi
#' @export
get_sample_df <- function(all_files, conn, run_props, run_error) {
  sample_df <- data.frame(file_name = character(0),
                          snprc_id = character(0),
                          cage = numeric(0),
                          bleed_date = character(0),
                          blood_expected = character(0),
                          blood_received = character(0),
                          pool_id = character(0))

  #total_number_of_files <- length(all_files)
  file_names <- basename(all_files)
  paths <- dirname(all_files)
  for (i in seq_along(all_files)) {
    file_name <- file_names[i]
    tmp_sample_df <- read_sample_file(stri_c(paths[i], "/", file_name),
                                      conn, run_props, run_error)
    sample_df <-
      rbind(sample_df,
            data.frame(file_name = rep(file_name, nrow(tmp_sample_df)),
                       tmp_sample_df))
  }
  sample_df[order(sample_df$file_name,
                  sample_df$bleed_date,
                  sample_df$cage,
                  sample_df$snprc_id), ]
}
#' Returns dataframe with animal Ids, cage location, bleed date, whether not a
#' blood sample was expected, whether or not a sample was received, and the
#' Pool Id number.
#'
#' @param path charcter vector of length 1 having the path section of the
#' fully qualified file name.
#' @import stringi
#' @export
get_pooled_sample_df <- function(path) {
  sample_df <- get_sample_df(path)
  sample_df[sample_df$pooled == "Y", ]
}
#' Returns the most recent assay file name for each plate number
#' @param file_id integer value of plate number
#' @param files_df dataframe with file_id, file name and file date.
#' @import stringi
#' @export
get_most_recent_file <- function(file_id, files_df) {
  files_subset <- files_df[files_df$file_id == file_id, ]
  files_subset <- files_subset[files_subset$start_date ==
                                 max(files_subset$start_date), ]
  if (nrow(files_subset) > 1) {
    files_subset <-
      files_subset[stri_detect(files_subset$file_name,
                               fixed = "reclassified"), ]
  }
  files_subset
}
#' Returns the most recent assay file name for each plate number
#'
#' @param files_all character vector of all file names.
#' @export
get_most_recent_files <- function(files_all) {
  if (length(files_all) == 0) {
    return(data.frame(file_id = character(0),
                      file_name = character(0),
                      start_date = character(0),
                      stringsAsFactors = FALSE))
  }
  files_df <- data.frame(file_id = get_file_ids(files_all),
                         file_name = files_all,
                         start_date = get_start_end_dates(files_all)$start_date,
                         stringsAsFactors = FALSE)

  most_recent_files <- data.frame(file_id = character(0),
                                  file_name = character(0),
                                  start_date = as.POSIXct(character()))
  for (file_id in unique(files_df$file_id)) {
    if (nrow(files_df[files_df$file_id == file_id, ]) > 1) {
      most_recent_files <-
        rbind(most_recent_files, get_most_recent_file(file_id, files_df))
    } else {
      most_recent_files <-
        rbind(most_recent_files, files_df[files_df$file_id == file_id, ])
    }
  }
  most_recent_files
}
#' Returns a logical value indicating whether or not the list of divisor
#' names in
#' the file ``cutoff.xlsx'' is the same as the list within the .lxd file.
#'
#' @param file character vector with the name of the name, including path,
#'  of the XML raw datafile
#' @import stringi
#' @export
divisors_match <- function(file) {
  divisor_values <- get_divisors(basename(as.character(file)))
  divisor_cat <- stri_c("after divisors_values ", file, " length = ",
                        length(divisor_values), "\n")
  region_df <- get_region_df(file, setup_name = "DefaultSetup")
  region_cat <- stri_c("after get_region_df ", file, " length = ",
                       nrow(region_df), "\n\n")
  result <- all(sort(names(divisor_values)) == sort(region_df$name))
  if (!result) {
    cat(divisor_cat)
    cat(region_cat)
  }
  result
}
#' Returns a charcter vector with the file names of files that have region
#' names that do not
#' match those found in ``cutoff.xlsx''
#'
#' @param file_names character vector of file names with XML data
#' @export
get_divisor_mismatch <- function(file_names) {
  file_list <- character(length(file_names))
  count <- 0
  for (file in file_names) {
    if (!divisors_match(file)) {
      count <- count + 1
      file_list[count] <- file
    }
  }
  file_list <- file_list[!is.na(file_list)]
}
#' Returns a named numeric vector with the divisors originally provided by the
#' vendor and copied into the Excel sheet "../inst/extdata/cutoff.xlsx"
#'
#' @param results character vector with results section of the Luminex file
#' being analyzed.
#' @import readxl
#' @import stringi
#' @export
get_excel_divisors <- function(results) {
  for (i in 1:nrow(results)) {
    if (any(tolower(results[i, ]) %in% "cutoff"))
      break
  }
  if (i >= nrow(results)) {
    stop(stri_c("Cutoff values were not found in get_excel_divisors().
                'cutoff' was not found in the ", nrow(results),
                " rows examined."))
  }
  start <- i
  row <- results[start, ]
  row[is.na(row)] <- ""
  name_col <- min((1:length(row))[stri_detect_fixed(tolower(row), "name")])
  cutoff_col <- (1:length(row))[stri_detect_fixed(tolower(row), "cutoff")]
  start <- start + 1
  results <- data.frame(results[start:nrow(results), ])
  rows <- 1:(which(is.na(results[ , name_col]))[1] - 1)
  #rows <- seq_along(results[ , name_col][!is.na(results[ , name_col])])
  divisors <- results[rows, cutoff_col]
  divisors <- as.numeric(divisors)
  divisor_names <- results[rows, name_col]
  if (!any(stri_detect_fixed(divisor_names, pattern = "rBV glycoB"))) {
    divisor_names[stri_detect_regex(tolower(divisor_names),
                                    pattern = "glyco")] <- "BV glyco B"
  }
  names(divisors) <- divisor_names
  divisors
}

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
#' Returns Plate[@@plateID] (plateID attribute of the Plate node.)
#'
#' @param lum XML object
#' @import XML
#' @export
get_plateID <- function(lum) {
  node <- getNodeSet(lum, "//Plate")
  if (length(node) > 1)
    stop(stri_c("More than one plate found in a file.", length(node),
                "were found."))
  as.character(sapply(node, xmlAttrs)[1])
}

#' Returns /WorkSet/Description from XML
#'
#' @param lum XML object
#' @import XML
#' @export
get_workset_desc <- function(lum) {
  node <- getNodeSet(lum, "/WorkSet/Description")
  xmlValue(node[[1]])
}

#' Returns date and time of assay based on '//Plate[@@name].
#'
#' @param lum XML object
#' @import stringi
#' @import XML
#' @export
get_session_date_tm <- function(lum) {
  node <- getNodeSet(lum, "//Plate")
  if (length(node) > 1)
    stop(stri_c("More than one plate found in a file.", length(node),
                "were found."))
  date_tm <- stri_sub(sapply(node, xmlAttrs)["name", 1], 8)
  stri_c(stri_sub(date_tm, 1, 4), "-", stri_sub(date_tm, 5, 6), "-",
         stri_sub(date_tm, 7, 8), " ", stri_sub(date_tm, 9, 10), ":",
         stri_sub(date_tm, 11, 12), ":", stri_sub(date_tm, 13, 14))
}
#' Returns the Setup[@@name]s for a lum
#'
#' @param lum XML object
#' @param setup indicates which Setup to return the name for. 0 indicates
#' return all setup names.

get_setup_name <- function(lum, setup = 0) {
  nodes <- getNodeSet(lum, stri_c("//Setup[@name]"))
  if (setup == 0) {
    setup_names <- character(length(nodes))
    for (i in seq_along(nodes)) {
      setup_names[i] <- sapply(nodes, xmlAttrs)[[i]]["name"]
    }
  } else {
    setup_names <- sapply(nodes, xmlAttrs)[[setup]]["name"]
  }
  setup_names
}
#' Returns a dataframe with region order, region name, and region id
#'
#' @param file name of file
#' @param setup_name value of name attribute of <Setup>
#' @import XML
#' @export
get_region_df <- function(file, setup_name = "DefaultSetup") {
  lum <- xmlTreeParse(file, getDTD = FALSE, useInternalNodes = TRUE)
  nodes <- getNodeSet(lum, stri_c(
    "//Setup[@name='", setup_name, "']/Region[@name]"))
  n <- length(nodes)
  regions <- sapply(nodes, xmlAttrs)
  region_names <- regions["name", ]
  region_ids <- regions["id", ]

  data.frame(order = (1:n), name = region_names, id = region_ids,
             stringsAsFactors = FALSE)
}

#' Returns a dataframe with the files, order, and Region[@@name]s
#' for a specific Setup, for example 'DefaultSetup'.
#'
#' @param files character vector of files to be processed
#' @param setup_name name of setup in XML file to get regions from
#' @export
get_all_regions <- function(files, setup_name = "DefaultSetup") {
  regions_df <- data.frame(file = character(0), order = integer(0),
                           name = character(0), id = character(0),
                           stringsAsFactors = FALSE)
  for (file in files) {
    regions_df <-
      rbind(regions_df, data.frame(file = basename(file),
                                   get_region_df(file, setup_name),
                                   stringsAsFactors = FALSE))
  }
  regions_df
}

#' Replaces an NA value with a character string
#'
#' @param x character vector
#' @param char character string used to replace NAs
#' @export
na_to_char <- function(x, char) {
  x[is.na(x)] <- char
  as.character(x)
}
#' Returns integer vector with plate numbers
#'
#' @param files name of file
#' @import stringi
#' @export
get_file_ids <- function(files) {
  if (all(stri_detect_fixed(tolower(files), "spfcolony"))) {
    file_ids <- unlist(stri_split(files,
                                  fixed = "plate", 2))[seq(2, length(files) * 2,
                                                           by = 2)]
    file_ids <- stri_split(file_ids, regex = "[_, .a-z,A-Z]", 2)
    file_ids <- unlist(file_ids)[seq(1, length(files) * 2, by = 2)]
  } else {
    file_ids <- sapply(stri_extract_all_charclass(files, "[0-9]", merge = TRUE),
                       function(x) {x[[1]]})
  }
  file_ids
}


#' Returns data frame with file and start_date as the two columns give
#' a list of luminex data file names
#'
#' Values are taken from '//Plate[@@startDate]"
#' @param files character vector with file names (with relative or absolute
#' path).
get_start_end_dates <- function(files) {
  start_dates <- sapply(files, function(file) {
    xmlAttrs(
      xmlChildren(xmlRoot(xmlTreeParse(file)))$Plate)[["startDate"]]
  })
  end_dates <- sapply(files, function(file) {
    xmlAttrs(
      xmlChildren(xmlRoot(xmlTreeParse(file)))$Plate)[["endDate"]]
  })
  data.frame(file = names(start_dates),
             start_date = strptime(as.character(start_dates),
                                   format = "%Y-%m-%dT%H:%M:%S"),
             end_date = strptime(as.character(end_dates),
                                 format = "%Y-%m-%dT%H:%M:%S"),
             stringsAsFactors = FALSE)
}
#' Returns dataframe of original values
#'
#' @param conn database connection object
#' @param file character vector with filename
#' @import XML
#' @export
get_raw_mfi <- function(conn, file) {
  lum <- xmlTreeParse(file, getDTD = FALSE, useInternalNodes = TRUE)
  nodes <- getNodeSet(lum, "/WorkSet//Plate//Well//LocName")
  well_names <- xmlSApply(nodes, xmlValue)
  animal_ids <- get_id_from_sample(conn, well_names)
  sample_dates <- get_date_from_sample(conn, well_names)
  report_date <- stri_split_fixed(get_session_date_tm(lum), " ")[[1]][[1]]
  report_dates <- rep(report_date, length(well_names))
  n_wells <- length(well_names)
  cat(stri_c("There are ", n_wells, " wells.\n"))
  # there is just one plate but it needs to be unwrapped.
  plate <- getNodeSet(lum, "/WorkSet//Plate")[[1]]
  region_df <- get_region_df(file)
  well_rows <- LETTERS[as.numeric(
    unlist(getNodeSet(plate, "//Well/@row"))) + 1]
  well_cols <- as.character(as.numeric(
    unlist(getNodeSet(plate, "//Well/@col"))) + 1)
  ids <- as.character(
    unlist(getNodeSet(plate, "//Well//RSts[@id != '0']/@id")))
  raw_mfi <- data.frame(
    file = rep(file, length(ids)),
    sample = rep(well_names, 1, each = nrow(region_df)),
    well_row = rep(well_rows, 1, each = nrow(region_df)),
    well_col = rep(well_cols, 1, each = nrow(region_df)),
    id = ids,
    animal_id = rep(animal_ids, 1, each = nrow(region_df)),
    sample_date = rep(sample_dates, 1, each = nrow(region_df)),
    report_date = rep(report_dates, 1, each = nrow(region_df)),
    name = rep(region_df$name, length(ids) / nrow(region_df)),
    val = as.numeric(unlist(getNodeSet(plate,
                                       "//Well//RSts[@id != '0']/@val"))),
    cv = as.numeric(unlist(getNodeSet(plate,
                                      "//Well//RSts[@id != '0']/@cv"))),
    stringsAsFactors = FALSE
  )
  raw_mfi$sd <- raw_mfi$val / raw_mfi$cv
  raw_mfi
}
#' Returns the mean_raw_mfi_df from an Excel file (.xlsx extension)
#'
#' @param conn database connection object
#' @param file fully qualified file name of Excel file
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import readxl
#' @import rmsutilityr
#' @import stringi
#' @export
get_mean_raw_mfi_from_xlsx <- function(conn, file, run_props, run_error) {
  sheet_names <- excel_sheets(file)
  if (any(stri_detect_regex(toupper(sheet_names), "^MAC"))) {
    if (any(stri_detect_fixed(toupper(sheet_names), "MFI AVERAGE"))) {
      ## It is a "raw" (XML renamed with MAC TRACK worksheet added) Excel file
      content <- read_excel(file, sheet = "MFI AVERAGE")
      col_names <- names(content)
      col_names[1] <- "sample"
      col_names[2] <- "wells"
      names(content) <- col_names
      content <- content[-1, ]
    } else {
      stop(stri_c("This file, (", basename(file), ") looks like a file that
                should have a MFI AVERAGE worksheet, but does not.
                  The worksheets found were ",
                  get_and_or_list(sheet_names), "."))
    }
  } else if (length(sheet_names) > 3) {
    stop(stri_c("This file, (", basename(file), ") looks like a file that
                should have a MAC TRACK worksheet, but does not.
                The worksheets found were ",
                get_and_or_list(sheet_names), "."))
  } else {# It is a Report style formated file
    content <- read_excel(file)
    content[1] <- stri_replace_all_regex(content[[1]], pattern = "\ ",
                                         replacement = "")
    #results <- get_result_tables(content)
    # repeats are already averaged in Excel
    content <- get_raw_mfi_tables(content)
    col_names <- as.character(content[1, ])
    col_names[1] <- "sample"
    col_names[2] <- "wells"
    col_names <- col_names[1:(which(col_names %in% c(NA, "NA"))[1] - 1)]
    content <- content[2:nrow(content), ]
    if (any(is.na(content[ , 1]))) {
      n_rows <- which(is.na(content[ , 1]))[1] - 1
    } else {
      n_rows <- length(as.character(content[ , 1]))
    }

    content <- content[2:n_rows, 1:length(col_names)]
    names(content) <- col_names
  }
  content <- content[!is.na(content$sample), ]
  get_raw_mfi_df(conn, content, file, run_props, run_error)
}
#' Returns the mean_raw_mfi_df from an XML file (.lxd extension) from the
#' Luminex machine.
#'
#' @param conn database connection object
#' @param file fully qualified file name of XML file
#' @export
get_mean_raw_mfi_from_lxd <- function(conn, file) {
  raw_mfi_df <- get_raw_mfi(conn, file) # has possible repeats
  get_mean_raw_mfi(raw_mfi_df) # returns mean of repeats
}
#' Returns a dataframe of file names, LocName, animal Id, and sample date and
#' has the side effect
#' of printing out the bad sample dates in a table with file name LocName
#'
#' @param conn database connection object
#' @param combined_df dataframe with agent assay data results and meta data
#' @param type character string indicating if the output is to be in "latex"
#' format or "html".
#' @param ... extra arguments to \code{xt_print()} that go to \code{print()}
#' @import animalr
#' @import rmsutilityr
#' @export
print_bad_sample_dates <- function(conn, combined_df, type = "latex", ...) {
  id_df <- data.frame(id = combined_df$snprc_id,
                      sample_date = combined_df$sample_date,
                      stringsAsFactors = FALSE)
  # If you do not have an Id or a date we cannot check.
  keep <- !(is.na(id_df$id) | is.na(id_df$sample_date))
  id_df <- id_df[keep, ]
  combined_df <- combined_df[keep, ]
  #cat('file = ', file, "\n")
  is_id <- is_animal_alive(conn, id_df)
  bad_sample_dates_df <-
    data.frame(file = character(0),
               sample = character(0),
               snprc_id = character(0),
               birth_date = character(0),
               colony = character(0),
               sample_date = character(0),
               stringsAsFactors = FALSE)
  if (any(!is_id)) {
    bad_sample_dates_df <-
      data.frame(file = basename(as.character(combined_df$file_name[!is_id])),
                 sample = combined_df$sample[!is_id],
                 snprc_id = combined_df$snprc_id[!is_id],
                 birth_date = combined_df$birth_date[!is_id],
                 colony = combined_df$colony[!is_id],
                 sample_date = combined_df$sample_date[!is_id],
                 stringsAsFactors = FALSE)
    bad_sample_dates_df <-
      bad_sample_dates_df[!duplicated(bad_sample_dates_df), ]
    if (nrow(bad_sample_dates_df) == 1) {
      caption <- stri_c(
        "There was 1 bad sample date identified where the animal was not
        present on the date indicated.")
    } else {
      caption <- stri_c(
        "There were ", nrow(bad_sample_dates_df), " bad sample dates identified
        where the animals were not present on the dates indicated.")
    }


    xt_print(bad_sample_dates_df, caption = caption,
      label = "tbl:bad-sample-dates", type = type, ...)
  }
  bad_sample_dates_df
}
#' Returns a dataframe of file names, sample, snprc_id, and sample_date and
#' has the side effect
#' of printing out the bad Ids in a table with file name LocName
#'
#' @param combined_df dataframe with agent assay data results and meta data
#' @param type character string indicating if the output is to be in "latex"
#' format or "html".
#' @param ... extra arguments to \code{xt_print()} that go to \code{print()}
#' @import animalr
#' @import rmsutilityr
#' @export
print_bad_animal_ids <- function(combined_df, type = "latex", ...) {
  controls <- c("ICL CNT High", "ICL CNT Low", "MAC Track High", "MAC Track High",
                "MAC Track Low", "Diluent CNT", "NHP CNT")

  samples <- unique(combined_df$sample[is.na(combined_df$snprc_id) &
                                         !combined_df$sample %in% controls])
  bad_animal_ids_df <- data.frame(file = character(0),
                                  sample = character(0),
                                  snprc_id = character(0),
                                  birth_date = character(0),
                                  colony = character(0),
                                  sample_date = character(0),
                                  stringsAsFactors = FALSE)
  bad_index <- seq_along(combined_df$sample)[combined_df$sample %in% samples]
  if (length(samples) > 0) {
    bad_animal_ids_df <-
      data.frame(
        file = basename(as.character(combined_df$file_name[bad_index])),
        sample = combined_df$sample[bad_index],
        snprc_id = combined_df$snprc_id[bad_index],
        birth_date = combined_df$birth_date[bad_index],
        colony = combined_df$colony[bad_index],
        sample_date = combined_df$sample_date[bad_index],
        stringsAsFactors = FALSE)

    bad_animal_ids_df <- bad_animal_ids_df[!duplicated(bad_animal_ids_df), ]
    if (length(samples) == 1) {
      caption <- stri_c(
        "There was 1 sample identified with an animal Id
        that could not be found in the animal database.")
    } else {
      caption <- stri_c(
      "There were ", length(samples), " samples (", length(unique(samples)), "
      unique samples) identified with animal Ids
        that could not be found in the animal database.")
    }

    xt_print(bad_animal_ids_df, caption = caption,
      label = "tbl:bad-animal-ids", type = type, ...)
  }

  bad_animal_ids_df
}
#' Returns bead based wide dataframe from bead based melted dataframe.
#'
#' @param .df bead based melted dataframe
#' @import reshape2
#' @import stringi
#' @export
get_melted_bead_2_wide <- function(.df) {
  bead_col <- get_col_names("bead")
  rows <- nrow(.df[.df$name == "SRV-2", ])
  cols <- nrow(.df) / rows
  .df <- data.frame(row = rep(1:rows, cols), .df)
  if (any(stri_detect_fixed(names(.df), "result"))) {
    value_var <- "result"
  } else {
    value_var <- "val"
  }
  w_df <- dcast(.df, row + file + sample + animal_id + sample_date +
                  report_date + wells ~
                  name, value.var = value_var)
  w_df <- w_df[order(w_df$row), ]
  w_df <-
    w_df[ , c("file", "sample",
              "animal_id", "sample_date", "report_date", "wells",
              bead_col[bead_col %in% names(w_df)])]
  w_df
}
#' Returns a list of dataframes that have been converted to wide format using
#' dcast
#'
#' @param mean_raw_mfi_df dataframe to be converted to wide format
#' @param mfi_df dataframe to be converted to wide format
#' @param d_mfi_df dataframe to be converted to wide format
#' @param r_mfi_df dataframe to be converted to wide format
#' @param combined_df dataframe to be converted to wide format
#'
#' @import reshape2
#' @export
get_wide_df <- function(mean_raw_mfi_df, mfi_df, d_mfi_df, r_mfi_df,
                        combined_df) {

  agent_col <- get_col_names("agent")
  ## Using "SRV AB" to count rows is not safe. It requires for
  ## "SRV AB" to be in the assay and in every well.
  rows <- nrow(combined_df[combined_df$test_name == "SRV AB", ])
  test_rows <- nrow(combined_df[combined_df$test_name == "SIV AB", ])
  if (rows != test_rows)
    stop("SRV AB rows != SIV AB rows in get_wide_df")
  cols <- nrow(combined_df) / rows
  combined_df <- data.frame(row = rep(1:rows, each = cols), combined_df)
  w_combined_df <-
    dcast(combined_df, row + file_name + plate_id + species +
            birth_date + colony + sample +
            snprc_id + sample_date + report_date +
            repeated + wells +
            procedure_name + procedure_id ~
            agent, value.var = "assay_value")
  w_combined_df <- w_combined_df[order(w_combined_df$row), ]
  w_combined_df <-
    w_combined_df[ , c("file_name", "plate_id", "species", "birth_date",
                       "colony", "sample",
                       "snprc_id", "sample_date",
                       "report_date",
                       "repeated", "wells", "procedure_name",
                       "procedure_id",
                       agent_col[agent_col %in% names(w_combined_df)])]
  list(w_mean_raw_mfi_df = get_melted_bead_2_wide(mean_raw_mfi_df),
       w_mfi_df = get_melted_bead_2_wide(mfi_df),
       w_d_mfi_df = get_melted_bead_2_wide(d_mfi_df),
       w_r_mfi_df = get_melted_bead_2_wide(r_mfi_df),
       w_combined_df = w_combined_df)
}
#' Returns a dataframe with four columns ("name", "value",
#' "java_data_type", and "location"), which is defined when a Run Properties
#' File provided by the system is read.
#'
#' @param properties_file character string with the file name provided by the
#' system as a replacement value for the macro \emph{${runInfo}}
#' @export
readRunPropertiesFile <- function(properties_file) {
  utils::read.table(properties_file, header = FALSE, sep = "\t",
             stringsAsFactors = FALSE,
             col.names = c("name", "value", "java_data_type", "location"),
             fill = TRUE)
}
#' Returns full name with path of transformed output file.
#'
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @export
getTransformedOutputFile <- function(run_props) {
  value <- NA
  if (any(run_props$name == "runDataFile")) {
    value <- run_props$location[run_props$name == "runDataFile"]
    # return NA for an empty string
    if (nchar(value) == 0) {
      value <- NA
    }
  }
  value
}
#' Returns value of run property if one is available else NA is provided.
#'
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param colName character vector of length 1 with the name of the property
#' whose value is sought.
#' @export
getRunPropertyValue <- function(run_props, colName) {
  value <- NA
  if (any(run_props$name == colName)) {
    value <- run_props$value[run_props$name == colName]
    # return NA for an empty string
    if (nchar(value) == 0) {
      value <- NA
    }
  }
  value
}
#' Returns integer value of error level (0:NONE, 1:WARN, 2:ERROR) used to set
#' run_error$level.
#'
#' If the new level is 2 then 2 is returned.
#' If the new level is the same as the current level and not 2, 0 is returned.
#' If the new level is greater than the current level and not 2 (that is if it
#' is 1) and the current severity level is not "ERROR", the new level (1) is
#' returned.
#' If the new level is 1 and the current severity level is "ERROR", 0 is
#' returned.

#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location") defined when a Run Properties File provided
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @param level integer value being used to reset run_error$level
#' @export
setMaxSeverity <- function(run_props, run_error, level) {
  max(run_error$level, level)
  # value <- 0
  #
  # # Don't display warnings if severityLevel set to ERROR
  # if (level == 2) {
  #   value <- 2
  # } else if (getRunPropertyValue(run_props, "severityLevel") != "ERROR" &&
  #            level > run_error$level) {
  #   value <- level
  # }
  # value
}
#' Returns  \code{run_error}, after setting \code{run_error$level} to 1 if
#' triggers warning and continues execution
#'
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @param msg character string containing the warning message
#' @import stringi
#' @export
triggerWarning <- function(run_props, run_error, msg) {
  run_error$level <- setMaxSeverity(run_props, run_error, level = 1)
  if (run_error$level < 2) {
    if (length(run_error$msg) == 0) {
      run_error$msg <- stri_c(msg, " Error level = ",
                              run_error$level)
    } else {
      run_error$msg <- stri_c(run_error$msg, "<br>", msg, " Error level = ",
                              run_error$level)
    }
    #    run_error$msg <-msg
  }
  run_error
}

#' Returns NULL: does not return -- triggers error and stops execution
#'
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @param msg character string containing the warning message
#' @import stringi
#' @export
triggerError <- function(run_props, run_error, msg) {
  run_error$level <- setMaxSeverity(run_props, run_error, level = 2)
  if (length(run_error$msg) == 0) {
    run_error$msg <- stri_c(msg, " Error level = ",
                            run_error$level)
  } else {
    run_error$msg <- stri_c(run_error$msg, "<br>", msg, " Error level = ",
                            run_error$level)
  }
  handleErrorsAndWarnings(run_props, run_error)
}
#' Returns NULL: does not return -- writes the maximumSeverity level to the
#' transformRunProperties file and the error message to the error.html file.
#'
#' LabKey server will read these files after execution to determine if an error
#' or warning occurred and handle it appropriately
#' @param run_props dataframe with four columns ("name", "value",
#' "java_data_type", and "location" defined when a Run Properties File provided
#' by the system is read by the \code{readRunPropertiesFile()} function.
#' @param run_error list of length 2 having the integer value of the error
#' level and the message to be displayed.
#' @import stringi
#' @export
handleErrorsAndWarnings <- function(run_props, run_error) {
  if (run_error$level > 0) {
    fileConn <- file(getRunPropertyValue(run_props,
                                         "transformedRunPropertiesFile"))
    if (run_error$level == 1) {
      writeLines(c(stri_c("maximumSeverity ", "WARN ", sep = "\t")), fileConn)
    }
    else {
      writeLines(c(stri_c("maximumSeverity ", "ERROR ", sep = "\t")), fileConn)
    }
    close(fileConn)

    # This file gets read and displayed directly as warnings or errors,
    # depending on maximumSeverity level.
    if (!is.null(run_error$msg)) {
      fileConn <- file("errors.html", open = "a")
      writeLines(run_error$msg, fileConn)
      close(fileConn)
    }
    quit()
  }
}
#' Finds any wells with low positive controls
#'
#' If either the \code{Human IgG} or the \code{Goat anti-human IgG} well
#' in the raw counts are below their respective cutoffs, the sample for that
#' well needs to be repeated.
#'
#' @param df dataframe with mean_raw_mfi_df format and data
#' @param divisors named numeric vector of divisors
#' @import stringi
#' @export
get_low_positive_controls <- function(df, divisors) {
  human_igg_divisor <-
    divisors[stri_detect_fixed(tolower(names(divisors)), "human igg") &
                !stri_detect_fixed(tolower(names(divisors)), "goat")]
  goat_anti_human_igg_divisor <-
    divisors[stri_detect_fixed(tolower(names(divisors)), "goat") &
                stri_detect_fixed(tolower(names(divisors)), " igg")]
  human_igg_vec <- (df$name == "Human IgG" &
                       !stri_detect_regex(df$sample, "iluent")) &
    df$val < human_igg_divisor
  goat_anti_human_igg_vec <- (df$name == "Goat anti-human IgG" &
                         !stri_detect_regex(df$sample, "iluent")) &
    df$val < goat_anti_human_igg_divisor
  log_vec <- human_igg_vec | goat_anti_human_igg_vec

  if (any(log_vec)) {
    low_positive_controls_df <- data.frame(
      file = df$file[log_vec],
      sample = df$sample[log_vec],
      sample_date = df$sample_date[log_vec],
      report_date = df$report_date[log_vec],
      animal_id = df$animal_id[log_vec],
      name = df$name[log_vec],
      wells = df$wells[log_vec],
      val = df$val[log_vec])
    low_positive_controls_df[!duplicated(low_positive_controls_df), ]
    low_positive_controls_df <-
      low_positive_controls_df[!is.na(low_positive_controls_df$sample), ]
  } else {
    low_positive_controls_df <- data.frame()
  }
  low_positive_controls_df
}
#' Get PDL results
#'
#' @return Dataframe with all contents of the pdl_results table except the
#' timestamp column.
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
get_pdl_results <- function(conn) {
  pdl_results_df <- sqlQuery(conn, "select * from pdl_results",
                             stringsAsFactors = FALSE)
  pdl_results_df$TIMESTAMP <- NULL
}
