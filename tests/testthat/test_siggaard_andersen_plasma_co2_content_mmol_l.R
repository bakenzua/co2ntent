context("siggaard_andersen_plasma_co2_content_mmol_l")

test_that("siggaard_andersen_plasma_co2_content_mmol_l returns correct number", {
  expect_equal(
    siggaard_andersen_plasma_co2_content_mmol_l(
      hco3_mmols_l = 17,
      pco2 = 5,
      pco2_units = "kPa"
    ),
    18.15509,
    tolerance = 0.000001
  )

  expect_equal(
    siggaard_andersen_plasma_co2_content_mmol_l(
      hco3_mmols_l = 17,
      pco2 = 37.5,
      pco2_units = "mmHg"
    ),
    18.155,
    tolerance = 0.000001
  )
})

test_that("siggaard_andersen_plasma_co2_content_mmol_l is vectorised", {
  hco3s = c(17, 17)
  pco2s = c(5, 5)
  expected <- c(18.15509, 18.15509)

  expect_equal(
    co2ntent:::siggaard_andersen_plasma_co2_content_mmol_l(
      hco3_mmols_l = hco3s,
      pco2 = pco2s,
      pco2_units = "kPa"
    ),
    expected,
    tolerance = 0.000001
  )
})
