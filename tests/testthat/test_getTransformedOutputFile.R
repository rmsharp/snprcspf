library(snprcspf)
context("getTransformedOutputFile")
run_props0 <-
  data.frame(name = c("severityLevel",
                      "runDataFile",
                      "runDataUploadedFile",
                      "runformedRunPropertiesFile"),
             value = c("", "","uploaded_data_file",
                       "properties_file"),
             java_data_type = c("java.lang.String", "java.lang.String",
                                "java.lang.String", "java.lang.String"),
             location = c("", "../data/input_file.csv",
                          "../data/uploaded_input_file.csv",
                          "run_properties_file.csv"),
             stringsAsFactors = FALSE)
test_that("getTransformedOutputFile returns NA if empty string is found", {
  expect_true(is.na(getTransformedOutputFile(run_props0)))})

run_props1 <-
  data.frame(name = c("severityLevel",
                      "runDataFile",
                      "runDataUploadedFile",
                      "runformedRunPropertiesFile"),
             value = c("", "data_file","uploaded_data_file",
                       "properties_file"),
             java_data_type = c("java.lang.String", "java.lang.String",
                                "java.lang.String", "java.lang.String"),
             location = c("", "../data/input_file.csv",
                          "../data/uploaded_input_file.csv",
                          "run_properties_file.csv"),
             stringsAsFactors = FALSE)

test_that("getTransformedOutputFile returns correct string", {
  expect_equal(getTransformedOutputFile(run_props1), "data_file")})

run_props2 <-
  data.frame(name = c("severityLevel",
                      "notArunDataFile",
                      "runDataUploadedFile",
                      "runformedRunPropertiesFile"),
             value = c("", "data_file","uploaded_data_file",
                       "properties_file"),
             java_data_type = c("java.lang.String", "java.lang.String",
                                "java.lang.String", "java.lang.String"),
             location = c("", "../data/input_file.csv",
                          "../data/uploaded_input_file.csv",
                          "run_properties_file.csv"),
             stringsAsFactors = FALSE)

test_that("getTransformedOutputFile returns NA is nothing is found", {
  expect_true(is.na(getTransformedOutputFile(run_props2)))})

