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
