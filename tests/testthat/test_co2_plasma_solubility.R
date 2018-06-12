context("co2_plasma_solubility")

test_that("co2_plasma_solubility returns correct number", {
  expect_equal(co2_plasma_solubility(37), 0.0307)
})
