context("haemoglobin_g_dl_param_check")

test_that("haemoglobin_g_dl_param_check raises warning on low value", {
  hbs = 1
  expect_warning(haemoglobin_g_dl_param_check(hbs))
  hbs = c(10, 10, 1)
  expect_warning(haemoglobin_g_dl_param_check(hbs))
})

test_that("haemoglobin_g_dl_param_check raises warning on high value", {
  hbs = 30
  expect_warning(haemoglobin_g_dl_param_check(hbs))
  hbs = c(10, 10, 30)
  expect_warning(haemoglobin_g_dl_param_check(hbs))
})

test_that("haemoglobin_g_dl_param_check raises error on missing value", {
  hbs = c(10, 10, NA)
  expect_error(haemoglobin_g_dl_param_check(hbs))
})
