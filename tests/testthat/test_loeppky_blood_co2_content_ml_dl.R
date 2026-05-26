context("loeppky_blood_co2_content_ml_dl")

test_that("loeppky_blood_co2_content_ml_dl returns correct number", {
  expect_equal(
    loeppky_blood_co2_content_ml_dl(pco2=5, pco2_units = "kPa") |> 
      unname(), 
    48.8328, 
    tolerance=0.00001
  )

  expect_equal(
    loeppky_blood_co2_content_ml_dl(pco2=37.503, pco2_units = "mmHg") |> 
      unname(), 
    48.8328, 
    tolerance=0.00001
  )
})