context("actual_bicarbonate_content_mmol_l")

test_that("actual_bicarbonate_content_mmol_l returns correct number", {
  expect_equal(
    co2ntent::actual_bicarbonate_content_mmol_l(
      pco2 = 5,
      ph = 7.4,
      pco2_units = "kPa"
    ),
    22.74763,
    tolerance = 0.000001
  )

  expect_equal(
    co2ntent::actual_bicarbonate_content_mmol_l(pco2 = 5),
    22.74763,
    tolerance = 0.000001
  )
})

test_that("actual_bicarbonate_content_mmol_l is vectorised", {
  pco2s <- c(5, 5, 6)
  expected <- c(22.74763, 22.74763295717454, 27.29716)

  expect_equal(
    co2ntent::actual_bicarbonate_content_mmol_l(
      pco2 = pco2s
    ),
    expected,
    tolerance = 0.000001
  )
})
