context("siggaard_andersen_blood_co2_content_mmol_l")

test_that("siggaard_andersen_blood_co2_content_mmol_l returns correct number", {
  expect_equal(
    siggaard_andersen_blood_co2_content_mmol_l(
      hco3_mmols_l = 17,
      pco2 = 5,
      haemoglobin_g_dl = 10,
      so2_fraction = 0.9,
      pco2_units = "kPa",
    ),
    17.1243,
    tolerance = 0.000001
  )
})

test_that("siggaard_andersen_blood_co2_content_mmol_l is vectorised", {
  hco3s = c(17, 17)
  pco2s = c(5, 5)
  hbs = c(10, 10)
  so2s = c(0.9, 0.9)
  expected <- c(17.1243, 17.1243)

  expect_equal(
    siggaard_andersen_blood_co2_content_mmol_l(
      hco3_mmols_l = hco3s,
      pco2 = pco2s,
      haemoglobin_g_dl = hbs,
      so2_fraction = so2s,
      pco2_units = "kPa",
    ),
    expected,
    tolerance = 0.000001
  )
})
