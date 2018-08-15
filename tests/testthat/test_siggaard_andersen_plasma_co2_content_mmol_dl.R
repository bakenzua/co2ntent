context("siggaard_andersen_plasma_co2_content_mmol_dl")

test_that("siggaard_andersen_plasma_co2_content_mmol_dl returns correct number", {
  expect_equal(siggaard_andersen_plasma_co2_content_mmol_dl(
    hco3_mmols_dl=17,
    pco2=5,
    inputs_are_kpa=TRUE
  ), 18.151344, tolerance=0.000001)
})

test_that("siggaard_andersen_plasma_co2_content_mmol_dl is vectorised", {
  hco3s = c(17,17)
  pco2s = c(5,5)
  expected <- c(18.151344, 18.151344)

  expect_equal(siggaard_andersen_plasma_co2_content_mmol_dl(hco3_mmols_dl=hco3s, pco2=pco2s), expected, tolerance=0.000001)
})
