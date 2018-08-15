context("douglas_blood_co2_content_ml_dl")

test_that("douglas_blood_co2_content_ml_dl returns correct number", {
  expect_equal(douglas_blood_co2_content_ml_dl(haemoglobin_g_dl=10, so2_fraction=0.9, pco2=5), 0.867593 * 54.803778, tolerance=0.000001)
})
