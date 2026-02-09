context("high-level facade API")

test_that("co2_content matches Douglas whole blood CO2 content (ml/dL)", {
  expect_equal(
    co2_content(
      phase  = "blood",
      method = "douglas",
      content_units  = "ml/dL",
      haemoglobin_g_dl = 10,
      so2_fraction     = 0.9,
      pco2             = 5,
      pco2_units = "kPa"
    ) |> unname(),
    47.55379,
    tolerance = 1e-5
  )
})

test_that("co2_content matches Siggaard-Andersen whole blood CO2 content (mmol/L)", {
  expect_equal(
    co2_content(
      phase  = "blood",
      method = "siggaard_andersen",
      content_units  = "mmol/L",
      hco3_mmols_l    = 17,
      pco2            = 5,
      haemoglobin_g_dl = 10,
      so2_fraction     = 0.9,
      pco2_units = "kPa"
    ),
    16.037097,
    tolerance = 1e-6
  )
})

test_that("o2_content matches blood_oxygen_content_mls_dl reference value", {
  expect_equal(
    o2_content(
      po2            = 10,
      so2_fraction   = 0.95,
      haemoglobin_g_dl = 15,
      po2_units = "kPa"
    ),
    18.8355,
    tolerance = 1e-6
  )
})

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

