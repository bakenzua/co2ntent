context("siggaard_andersen_plasma_bicarbonate_content_mmol_l")

test_that("siggaard_andersen_plasma_bicarbonate_content_mmol_l returns correct number", {
  expect_equal(siggaard_andersen_plasma_bicarbonate_content_mmol_l(
    pco2=5,
    ph=7.4,
    temperature=37,
    inputs_are_kpa=TRUE

  ), 22.846537295717454, tolerance=0.000001)

  expect_equal(siggaard_andersen_plasma_bicarbonate_content_mmol_l(pco2=5), 22.846537295717454, tolerance=0.000001)
})

test_that("siggaard_andersen_plasma_bicarbonate_content_mmol_l is vectorised", {
  pco2s <- c(5,5,6)
  expected <- c(22.846537295717454, 22.846537295717454, 27.415844)

  expect_equal(siggaard_andersen_plasma_bicarbonate_content_mmol_l(pco2=pco2s), expected, tolerance=0.000001)
})
