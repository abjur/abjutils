context("Check digits.")

test_that("calc_dig is the check digit of a process code.", {
  expect_equal(calc_dig("001040620018260004", build = TRUE), "00104064020018260004")
  expect_equal(calc_dig("001040620018260004", build = FALSE), "40")
})

context("Check digit verification.")

test_that("check_dig verifies if the check digit is well formed.", {
  expect_equal(check_dig("0005268-75.2013.8.26.0100"), TRUE)
  expect_equal(check_dig("0010406-40.2001.8.26.0004"), TRUE)
  expect_equal(check_dig("0010406-50.2001.8.26.0004"), FALSE)
})
