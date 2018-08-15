context("so2_fraction_param_check")

test_that("so2_fraction_param_check raises warning on low value", {
  so2s = -1
  expect_warning(so2_fraction_param_check(so2s))
  so2s = c(0.5, 0.5, -1)
  expect_warning(so2_fraction_param_check(so2s))
})

test_that("so2_fraction_param_check raises warning on high value", {
  so2s = 2
  expect_warning(so2_fraction_param_check(so2s))
  so2s = c(0.5, 0.5, 2)
  expect_warning(so2_fraction_param_check(so2s))
})

test_that("so2_fraction_param_check raises error on missing value", {
  so2s = c(0.5, 0.5, NA)
  expect_error(so2_fraction_param_check(so2s))
})
