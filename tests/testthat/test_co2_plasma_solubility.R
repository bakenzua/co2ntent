context("co2_plasma_solubility")

test_that("co2_plasma_solubility returns correct number", {
  expect_equal(co2_plasma_solubility(37), 0.0307)
})

test_that("co2_plasma_solubility is vectorised", {
  temp_vector <- c(37, 37)
  expect_equal(co2_plasma_solubility(temp_vector), c(0.0307, 0.0307))
})

test_that("co2_plasma_solubility warns for small and large temperatures", {
  expect_warning(co2_plasma_solubility(temperature = 22))
  expect_warning(co2_plasma_solubility(temperature = 45))
})

test_that("co2_plasma_solubility errors for missing values", {
  ts <- c(37, NA)
  expect_error(co2_plasma_solubility(temperature = ts))
})
