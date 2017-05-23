library(snprcspf)
library(RODBC)
library(rmsutilityr)
conn <- odbcConnect("hellcat-miami-animal-sa")
context("Get Ids from samples tests")
samples1 <- c("ICL CNT High", "ICL CNT Low", "NHP CNT (ICL)", "Diluent CNT (ICL)",
              "31887 - 05/21/2014", "29461 - 05/28/2014", "30923 - 06/03/2014",
              "16650 - 06/04/2014", "28240 - 06/04/2014", "28794 - 06/11/2014",
              "28935 - 06/11/2014", "31155 - 07/01/2014", "28168 - 07/08/2014",
              "30003 - 07/15/2014", "30125 - 07/15/2014", "19702 - 07/22/2014",
              "16352 - 07/23/2014", "16649 - 07/23/2014", "31316 - 07/29/2014",
              "31147 - 08/06/2014", "31162 - 08/06/2014", "31729", "31459",
              "14824", "16716", "ICL CNT High-8-28-14", "ICL CNT Low-8-28-14"
)
samples2 <- c("20161208-31543",
             "20161208-01553",
             "20161208-31555",
             "20161208-31650",
             "20161208-31720",
             "20161208-31785",
             "31729",
             "32623 - 07/09/2014",
             "20160713-33458-R",
             "20161208-31848",
             "ICL CNT High",
             "ICL CNT Low",
             "NHP CNT (ICL)",
             "Diluent CNT (ICL)")
test_that("All sample formats with Ids produce Ids and those without do not", {
  expect_equal(
    blank_fill_ids(get_id_from_sample(conn, samples1)), c(NA, NA, NA, NA,
                                          " 31887", " 29461", " 30923",
                                          " 16650", " 28240", " 28794",
                                          " 28935", " 31155", " 28168",
                                          " 30003", " 30125", " 19702",
                                          " 16352", " 16649", " 31316",
                                          " 31147", " 31162", " 31729",
                                          " 31459", " 14824", " 16716",
                                          NA, NA))
  expect_equal(
    blank_fill_ids(get_id_from_sample(conn, samples2)),
    c(" 31543", NA, " 31555", " 31650", " 31720", " 31785", " 31729",
      " 32623", " 33458", " 31848", NA, NA, NA, NA))
})
odbcClose(conn)
