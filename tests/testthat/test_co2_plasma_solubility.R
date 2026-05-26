context("douglas_co2_plasma_solubility")

test_that("douglas_co2_plasma_solubility returns correct number", {
  expect_equal(co2ntent:::douglas_co2_plasma_solubility(37), 0.0307)
})

test_that("douglas_co2_plasma_solubility is vectorised", {
  temp_vector <- c(37, 37)
  expect_equal(co2ntent:::douglas_co2_plasma_solubility(temp_vector), c(0.0307, 0.0307))
})
