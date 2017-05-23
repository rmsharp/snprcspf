library(snprcspf)
context("get_report_date_from_file_name_test")
file_names <- c(
  "../data/new//Report plate 55 03-21-2014.xlsx",
  "../data/new/spfcolonyplate54.lxd",
  "../data/new//Report plate 68 01-23-2015b.xlsx",
  "../data/new//Report plate 69 02-06-2015.xlsx",
  "../data/new//Report Plate 76  06-09-15 b.xlsx",
  "../data/new//Report Plate 75  05-13-15.xlsx",
  "../data/SPF Colony Testing Plate 89   02-16-16.xlsx",
  "../data/SPF Colony Testing Plate 89   02-16-16-1.xlsx"
  )
test_that("get_report_date_from_file_name returns either correct date or NA", {
  expect_equal(get_report_date_from_filename(file_names),
               c("2014-03-21", NA, "2015-01-23", "2015-02-06", "2015-06-09",
                 "2015-05-13", "2016-02-16", "2016-02-16"))})
