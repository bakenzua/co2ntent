context("douglas_blood_co2_content_ml_dl")

test_that("douglas_blood_co2_content_ml_dl returns correct number", {
  expect_equal(
    douglas_blood_co2_content_ml_dl(haemoglobin_g_dl=10, so2_fraction=0.9, pco2=5) |> 
      unname(), 
    47.55379, 
    tolerance=0.00001
  )
})
