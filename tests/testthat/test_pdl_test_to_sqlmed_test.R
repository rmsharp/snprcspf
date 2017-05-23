library(snprcspf)
context("pdl tests to SqlMed procedures and tests")
procedure_names <- c(
  "SIV PCR" = "VIRAL ANTIBODY SCREEN",
  "SIV WB" = "VIRAL ANTIBODY SCREEN",
  "SRV PCR" = "VIRAL ANTIBODY SCREEN",
  "SRV1 WB" = "VIRAL ANTIBODY SCREEN",
  "STLV PCR" = "STLV-1 BY PCR",
  "STLV WB" = "VIRAL ANTIBODY SCREEN")
test_that("pdl_test converts to sqlmed_procedure_name", {
  expect_equal(pdl_assay_to_sqlmed_procedure_name(names(procedure_names)),
    as.character(procedure_names))
 })

procedure_ids <- c(
   "SIV PCR" = 10616,
   "SIV WB" = 10616,
   "SRV PCR" = 10616,
   "SRV1 WB" = 10616,
   "STLV PCR" = 10620,
   "STLV WB" = 10616)
test_that("pdl_test converts to sqlmed_procedure_id", {
   expect_equal(pdl_assay_to_sqlmed_procedure_id(
     names(procedure_ids)),
                as.integer(procedure_ids))
})
test_name <- c(
 "SIV PCR" = "SIV PCR",
 "SIV WB" = "SIV WB",
 "SRV PCR" = "SRV PCR",
 "SRV1 WB" = "SRV1 WB",
 "STLV PCR" = "STLV-1 BY PCR",
 "STLV WB" = "STLV WB")
test_that("pdl_test converts to sqlmed_test_name", {
  expect_equal(pdl_assay_to_sqlmed_test_name(names(test_name)),
               as.character(test_name))
})
test_id <- c(
  "SIV PCR" = 852,
  "SIV WB" = 948,
  "SRV PCR" = 875,
  "SRV1 WB" = 943,
  "STLV PCR" = 898,
  "STLV WB" = 947)
test_that("pdl_test converts to sqlmed_test_id", {
  expect_equal(pdl_assay_to_sqlmed_test_id(names(test_id)),
               as.integer(test_id))
})
