context("ph_param_check")

test_that("ph_param_check raises warning on low value", {
  phs = 6
  expect_warning(ph_param_check(phs))
  phs = c(7.4, 7.4, 6)
  expect_warning(ph_param_check(phs))
})

test_that("ph_param_check raises warning on high value", {
  phs = 8
  expect_warning(ph_param_check(phs))
  phs = c(7.4, 7.4, 8)
  expect_warning(ph_param_check(phs))
})

test_that("ph_param_check raises error on missing value", {
  phs = c(7.4, 7.4, NA)
  expect_error(ph_param_check(phs))
})
