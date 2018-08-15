context("bicarbonate_mmol_dl_param_check")

test_that("bicarbonate_mmol_dl_param_check raises warning on low value", {
  hco3 = 0.5
  expect_warning(bicarbonate_mmol_dl_param_check(hco3))
  hco3 = c(25,25,25,0.5)
  expect_warning(bicarbonate_mmol_dl_param_check(hco3))
})

test_that("bicarbonate_mmol_dl_param_check raises warning on high value", {
  hco3 = 100
  expect_warning(bicarbonate_mmol_dl_param_check(hco3))
  hco3 = c(25,25,25,100)
  expect_warning(bicarbonate_mmol_dl_param_check(hco3))
})

test_that("bicarbonate_mmol_dl_param_check raises error on missing value", {
  hco3 = c(25,25,25,NA)
  expect_error(bicarbonate_mmol_dl_param_check(hco3))
})
