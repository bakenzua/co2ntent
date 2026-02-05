context("douglas_plasma_co2_content_ml_dl")

test_that("douglas_plasma_co2_content_ml_dl returns correct number", {
  expect_equal(douglas_plasma_co2_content_ml_dl(pco2=5, inputs_are_kpa = TRUE) |> unname(), 
  54.81116, 
  tolerance=0.000001
  )
})
