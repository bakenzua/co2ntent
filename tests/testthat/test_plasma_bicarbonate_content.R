context("plasma_bicarbonate_content")

test_that("plasma_bicarbonate_content returns correct number", {
  expect_equal(plasma_bicarbonate_content(
    pco2=5,
    ph=7.4,
    temperature=37,
    calculate_solubitity_coefficient=FALSE,
    inputs_are_kpa=TRUE

  ), 17.210552, tolerance=0.000001)

  expect_equal(plasma_bicarbonate_content(pco2=5), 17.210552, tolerance=0.000001)
})

test_that("plasma_bicarbonate_content is vectorised", {
  pco2s <- c(5,5,6)
  expected <- c(17.210552, 17.210552, 20.652663)

  expect_equal(plasma_bicarbonate_content(pco2=pco2s), expected, tolerance=0.000001)
})
