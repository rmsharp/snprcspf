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
