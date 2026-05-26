context("mmhg_to_kpa")

test_that("mmhg_to_kpa returns correct number", {
  expect_equal(mmhg_to_kpa(7.5), 0.9999179, tolerance=0.000001)
})
