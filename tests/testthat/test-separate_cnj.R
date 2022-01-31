
context("separate")

test_that("separate_cnj works", {
  library(magrittr)
  da <- data.frame(
    a = "1000315-72.2016.8.26.0156"
  )


  testthat::expect_warning(resp <- separate_cnj(da, a), regexp = NA)

  testthat::expect_equal(class(resp), "data.frame")
  testthat::expect_equal(dim(resp), c(1, 6))
})
