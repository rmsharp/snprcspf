library(snprcspf)
##library(testthat)
##library(stringi)
context("get_yyyymmdd_from_possible_dates")
samples <- c(
  "2015-09-23 - 33983",
  "20150923-33983",
  "31016 -5/22/15",
  "30848 - 09/17/2014",
  "31271 - 09/5/2014",
  "09/5/2014 - 31271")
possible_dates <- c(
  "2015-09-23",
  "20150923",
  "5/22/15",
  "30848",
  "09/17/2014",
  "09/5/2014",
  "09/5/2014")
test_that("get_date_from_possible", {
  expect_equal(get_yyyymmdd_from_possible_dates(possible_dates),
               c("2015-09-23", "2015-09-23", "2015-05-22", "30848", "2014-09-17",
                 "2014-09-05", "2014-09-05"))})
