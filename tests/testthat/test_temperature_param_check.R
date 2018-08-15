context("temperature_param_check")

test_that("temperature_param_check raises warning on low value", {
  temperatures = 25
  expect_warning(temperature_param_check(temperatures))
  temperatures = c(37,37,25)
  expect_warning(temperature_param_check(temperatures))
})

test_that("temperature_param_check raises warning on high value", {
  temperatures = 45
  expect_warning(temperature_param_check(temperatures))
  temperatures = c(37,37,45)
  expect_warning(temperature_param_check(temperatures))
})

test_that("temperature_param_check raises error on missing value", {
  temperatures = c(37,37,NA)
  expect_error(temperature_param_check(temperatures))
})
