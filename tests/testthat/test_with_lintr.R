library(snprcspf)
context("Running lintr on code")
if (requireNamespace("lintr", quietly = TRUE)) {
  skip_on_cran()
  skip_on_travis()
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
