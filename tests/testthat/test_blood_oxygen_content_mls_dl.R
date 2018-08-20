context("blood_oxygen_content_mls_dl")

test_that("blood_oxygen_content_mls_dl returns correct number", {
  expect_equal(blood_oxygen_content_mls_dl(
                                           po2=10,
                                           so2_fraction=0.95,
                                           haemoglobin_g_dl=15,
                                           inputs_are_kpa = TRUE
                                          ),
               18.8355, tolerance=0.000001)
})

test_that("blood_oxygen_content_mls_dl returns correct number for mmHg", {
  expect_equal(blood_oxygen_content_mls_dl(
    po2=75.006157584565642,
    so2_fraction=0.95,
    haemoglobin_g_dl=15,
    inputs_are_kpa = FALSE
  ),
  18.8355, tolerance=0.000001)
})


test_that("blood_oxygen_content_mls_dl returns is vectorised", {
 po2s <- c(10, 10, 10, 10, 10)
 so2s <- c(0.95, 0.95, 0.95, 0.95, 0.95)
 hbs <- c(15, 15, 15, 15, 15)
 expected <- c(18.8355, 18.8355, 18.8355, 18.8355, 18.8355)
 expect_equal(blood_oxygen_content_mls_dl(po2=po2s, so2_fraction=so2s, haemoglobin_g_dl=hbs), expected, tolerance=0.000001)
})
