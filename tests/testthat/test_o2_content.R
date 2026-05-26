context("Test o2_content")

test_that("o2_content matches blood_oxygen_content_mls_dl reference value", {
  expect_equal(
    o2_content(
      po2            = 10,
      so2_fraction   = 0.95,
      haemoglobin_g_dl = 15,
      pressure_units = "kPa"
    ),
    18.8355,
    tolerance = 1e-6
  )
})



