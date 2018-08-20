context("po2_param_check")

test_that("po2_param_check raises warning on low value", {
  po2s_kpa = 0.5
  expect_warning(po2_param_check(po2s_kpa))
  po2s_kpa = c(5,5,5,0.5)
  expect_warning(po2_param_check(po2s_kpa))
  pco2s_mmHg = 3
  expect_warning(po2_param_check(pco2s_mmHg, inputs_are_kpa=FALSE))
})

test_that("po2_param_check raises warning on high value", {
  po2s_kpa = 150
  expect_warning(po2_param_check(po2s_kpa))
  po2s_kpa = 86
  expect_failure(expect_warning(po2_param_check(po2s_kpa)))
  po2s_kpa = c(5,5,5,150)
  expect_warning(po2_param_check(po2s_kpa))
  pco2s_mmHg = 1125
  expect_warning(po2_param_check(pco2s_mmHg, inputs_are_kpa=FALSE))
})

test_that("po2_param_check raises error on missing value", {
  po2s_kpa = c(5,5,5,NA)
  expect_error(po2_param_check(po2s_kpa))
})
