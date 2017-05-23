library(snprcspf)
library(stringi)
context("RunProperties tests")
run_props <-
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
test_that("getRunPropertyValue returns selected value", {
  expect_equal(
    getRunPropertyValue(run_props, "severityLevel"), NA)
  expect_equal(
    getRunPropertyValue(run_props, "runDataFile"), "data_file")
  expect_equal(
    getRunPropertyValue(run_props, "runDataUploadedFile"), "uploaded_data_file")
  expect_equal(
    getRunPropertyValue(run_props, "runformedRunPropertiesFile"),
    "properties_file")
})
test_that("setMaxSeverity returns correct value", {
  return_value <- c(c(0, 1, 2), c(1, 1, 2), c(2, 2, 2), c(0, 1, 2),
                    c(1, 1, 2), c(2, 2, 2))
  ptr <- 0
  #for (severityLevel in c("", "WARN", "ERROR")) {
  for (severityLevel in c("WARN", "ERROR")) {
      run_props$value[run_props$name == "severityLevel"] <- severityLevel
    for (error_level in c(0, 1, 2)) {
      if (error_level == 0) {
        msg <- ""
      } else if (error_level == 1) {
        msg <- "warning message"
      } else if (error_level == 2) {
        msg <- "error message"
      } else {
        stop("error_level is not 0, 1, or 2")
      }
      run_error <- list(level = error_level, msg = msg)
      for (set_value in c(0, 1, 2)) {
        ptr <- ptr + 1
        # print(stri_c("ptr = ", ptr, "; severityLevel = ", severityLevel,
        #              "; error_level = ", error_level,
        #              "; set_value = ", set_value,
        #              "; return_value = ",
        #              setMaxSeverity(run_props, run_error, set_value)))

        expect_equal(
          setMaxSeverity(run_props, run_error, set_value), return_value[ptr])
      }
    }
  }
})
test_that("triggerWarning set the msg when it is supposed to", {
  run_errors <- data.frame(level = c(0, 1, 2),
                           msg = c("nothing wrong", "warning sent",
                                   "error found"),
                           stringsAsFactors = FALSE)
  #for (severityLevel in c("", "WARN", "ERROR")) {
  for (severityLevel in c("WARN", "ERROR")) {
    run_props$value[run_props$name == "severityLevel"] <- severityLevel
    for (i in 1:3) {
      run_error <- run_errors[i, ]
      # print(
      #   stri_c("severityLevel = ", severityLevel,
      #          "; run_error$level = ", run_error$level,
      #          "; run_error$msg = ", run_error$msg,
      #          '; triggerWarning(run_props, run_error, "new_message")$msg = ',
      #          triggerWarning(run_props, run_error, "new_message")$msg))
      if (run_error$level == 2) {
        expect_equal(triggerWarning(run_props, run_error, "new_message")$msg,
                     "error found")
      } else {
        expect_equal(triggerWarning(run_props, run_error, "new_message")$msg,
                     stri_c(run_error$msg, "<br>new_message Error level = 1"))
      }
    }
  }
})

