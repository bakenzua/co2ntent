context("siggaard_andersen_plasma_co2_content_mmol_l")

test_that("siggaard_andersen_plasma_co2_content_mmol_l returns correct number", {
  expect_equal(siggaard_andersen_plasma_co2_content_mmol_l(
    hco3_mmols_l=17,
    pco2=5,
    inputs_are_kpa=TRUE
  ), 18.149999, tolerance=0.000001)
})

test_that("siggaard_andersen_plasma_co2_content_mmol_l is vectorised", {
  hco3s = c(17,17)
  pco2s = c(5,5)
  expected <- c(18.149999, 18.149999)

  expect_equal(siggaard_andersen_plasma_co2_content_mmol_l(hco3_mmols_l=hco3s, pco2=pco2s), expected, tolerance=0.000001)
})
