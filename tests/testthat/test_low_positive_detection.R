library(snprcspf)
library(stringi, quietly = TRUE)
context("Low positive detection tests")
sample_name1 <- "20151013-34234"
sample_name2 <- "ICL CNT High"
sample_name3 <- "ICL CNT Low"
sample_name4 <- "NHP CNT (ICL)"
sample_name5 <- "Diluent CNT (ICL)"
sample_name6 <- "ICL CNT High-Lot E"
sample_name7 <- "ICL CNT Low- Lot E"
sample_name8 <- "20150923-33948-R"
test_that("'ICL CNT Low- Lot E' not confused with 'ICL CNT Low'", {
  expect_true(stri_detect_fixed(tolower(sample_name3), pattern = "low") &
                !stri_detect_regex(tolower(sample_name3), pattern = "lot"))
  expect_false(stri_detect_fixed(tolower(sample_name7), pattern = "low") &
                !stri_detect_regex(tolower(sample_name7), pattern = "lot"))
})
test_that("'Diluent CNT (ICL)' not confused with 'ICL CNT Low'", {
  expect_true(stri_detect_fixed(tolower(sample_name3), pattern = "low") &
                !stri_detect_regex(tolower(sample_name3), pattern = "lot"))
  expect_false(stri_detect_fixed(tolower(sample_name5), pattern = "low") &
                 !stri_detect_regex(tolower(sample_name5), pattern = "lot"))
})
test_that("'ICL CNT High' not confused with 'ICL CNT Low'", {
  expect_true(stri_detect_fixed(tolower(sample_name3), pattern = "low") &
                !stri_detect_regex(tolower(sample_name3), pattern = "lot"))
  expect_false(stri_detect_fixed(tolower(sample_name2), pattern = "low") &
                 !stri_detect_regex(tolower(sample_name2), pattern = "lot"))
})
test_that("'20150923-33948-R' not confused with 'ICL CNT Low'", {
  expect_true(stri_detect_fixed(tolower(sample_name3), pattern = "low") &
                !stri_detect_regex(tolower(sample_name3), pattern = "lot"))
  expect_false(stri_detect_fixed(tolower(sample_name8), pattern = "low") &
                 !stri_detect_regex(tolower(sample_name8), pattern = "lot"))
})

