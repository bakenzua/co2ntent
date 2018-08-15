context("pco2_param_check")

test_that("pco2_param_check raises warning on low value", {
  pco2s_kpa = 0.5
  expect_warning(pco2_param_check(pco2s_kpa))
  pco2s_kpa = c(5,5,5,0.5)
  expect_warning(pco2_param_check(pco2s_kpa))
  pco2s_mmHg = 3
  expect_warning(pco2_param_check(pco2s_mmHg, inputs_are_kpa=FALSE))
})

test_that("pco2_param_check raises warning on high value", {
  pco2s_kpa = 22
  expect_warning(pco2_param_check(pco2s_kpa))
  pco2s_kpa = c(5,5,5,22)
  expect_warning(pco2_param_check(pco2s_kpa))
  pco2s_mmHg = 200
  expect_warning(pco2_param_check(pco2s_mmHg, inputs_are_kpa=FALSE))
})

test_that("pco2_param_check raises error on missing value", {
  pco2s_kpa = c(5,5,5,NA)
  expect_error(pco2_param_check(pco2s_kpa))
})
