library(snprcspf)
context("Date conversion tests")
test_that("mm-dd-yyyy converts to yyyy-mm-dd", {
  expect_equal(
    mdy_to_yyyymmdd(c("1-21-15", "2/13/98", "05-28-2014"), sep = "-"),
    c( "2015-01-21", "1998-02-13", "2014-05-28"))
  expect_equal(
    mdy_to_yyyymmdd(c("1-21-15", "2/13/98", "05-28-2014"), sep = ""),
    c( "20150121", "19980213", "20140528"))
  expect_equal(mdy_to_yyyymmdd(c("1-21-15", "2/13/98", "05-28-2014")),
               c( "20150121", "19980213", "20140528"))
  expect_equal(
    mdy_to_yyyymmdd(c("1-21-15", "2/13/98", "05-28-2014"), sep = "/"),
    c( "2015/01/21", "1998/02/13", "2014/05/28"))
  expect_equal(
    pooled_bleed_date_to_sample_date(
      data.frame(bleed_date = c("20150121", "19980213",
                                "20140528")), sep = "-")$sample_date,
    c("2015-01-21", "1998-02-13", "2014-05-28"))
})
