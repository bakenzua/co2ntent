context("co2_content")

test_that("co2_content matches Douglas whole blood CO2 content (ml/dL)", {
  expect_equal(
    co2_content(
      phase = "blood",
      method = "douglas",
      content_units = "ml/dL",
      haemoglobin_g_dl = 10,
      so2_fraction = 0.9,
      pco2 = 5,
      pco2_units = "kPa"
    ) |>
      unname(),
    47.55379,
    tolerance = 1e-5
  )
})

test_that("co2_content matches Loeppky whole blood CO2 content (ml/dL)", {
  expect_equal(
    co2_content(
      phase = "blood",
      method = "loeppky",
      content_units = "ml/dL",
      pco2 = 5,
      pco2_units = "kPa"
    ) |>
      unname(),
    48.8328, # loeppky_blood_co2_content_ml_dl(pco2=5, pco2_units = "kPa"),
    tolerance = 1e-5
  )
})

test_that("co2_content matches Siggaard-Andersen whole blood CO2 content (mmol/L)", {
  expect_equal(
    co2_content(
      phase = "blood",
      method = "siggaard_andersen",
      content_units = "mmol/L",
      hco3_mmols_l = 17,
      pco2 = 5,
      haemoglobin_g_dl = 10,
      so2_fraction = 0.9,
      pco2_units = "kPa"
    ),
    17.1243,
    tolerance = 1e-6
  )
})
