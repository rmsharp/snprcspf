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
