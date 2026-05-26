context("Test po2_to_so2()")

test_that("po2_to_so2 matches kelman_po2_to_so2 reference value", {
  expect_equal(
    po2_to_so2(
      po2            = 10,
      temperature    = 37,
      ph             = 7.4,
      pco2           = 5.3329,
      pressure_units = "kPa"
    ),
    0.950795,
    tolerance = 1e-6
  )
})