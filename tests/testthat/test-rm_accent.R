context("Test removal of non-ASCII characters.")

test_that("rm_accent is the converted to non-ASCII characters of a string.", {
  expect_equal(rm_accent("áéíóú ê fáfé@"), "aeiou e fafe@")
  expect_equal(rm_accent("-_pê pãn"), "-_pe pan")
})