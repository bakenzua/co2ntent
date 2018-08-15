context("kpa_to_mmhg")

test_that("kpa_to_mmhg returns correct number", {
  expect_equal(kpa_to_mmhg(1), 7.50061575845656, tolerance=0.000001)
})
