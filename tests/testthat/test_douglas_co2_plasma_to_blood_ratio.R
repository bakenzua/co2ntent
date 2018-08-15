context("douglas_co2_plasma_to_blood_ratio")

test_that("douglas_co2_plasma_to_blood_ratio returns correct number", {
  expect_equal(douglas_co2_plasma_to_blood_ratio(haemoglobin_g_dl=10, so2_fraction=0.9), 0.867593, tolerance=0.000001)
})
