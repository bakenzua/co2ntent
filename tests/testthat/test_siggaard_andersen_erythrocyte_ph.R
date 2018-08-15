context("siggaard_andersen_erythrocyte_ph")

test_that("siggaard_andersen_erythrocyte_ph returns correct number", {
  expect_equal(siggaard_andersen_erythrocyte_ph(0.9), 7.1935, tolerance=0.000001)
})

test_that("siggaard_andersen_erythrocyte_ph is vectorised", {
  so2s <- c(0.9, 0.9)
  expected <- c(7.1935, 7.1935)

  expect_equal(siggaard_andersen_erythrocyte_ph(so2_fraction=so2s), expected, tolerance=0.000001)
})
