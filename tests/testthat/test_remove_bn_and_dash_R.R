library(snprcspf)
context("remove-bn-and-dash-R")
test_that("bn and -R are stripped off of sample", {
  expect_equal(remove_bn_and_dash_R(c("Bn1X4012 - 02-13-2014")),
               "1X4012 - 02-13-2014")
  expect_equal(remove_bn_and_dash_R(c("Bn1X4012 - 02-13-2014",
                                      "Bn12345-05-21-2014-R")),
               c("1X4012 - 02-13-2014", "12345-05-21-2014"))
})
