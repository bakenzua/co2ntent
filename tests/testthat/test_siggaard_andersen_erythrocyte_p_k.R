context("siggaard_andersen_erythrocyte_p_k")

test_that("siggaard_andersen_erythrocyte_p_k returns correct number", {
  expect_equal(siggaard_andersen_erythrocyte_p_k(0.9), 6.046073, tolerance=0.000001)
})

test_that("siggaard_andersen_erythrocyte_p_k is vectorised", {
  so2s <- c(0.9, 0.9)
  expected <- c(6.046073, 6.046073)

  expect_equal(siggaard_andersen_erythrocyte_p_k(so2_fraction=so2s), expected, tolerance=0.000001)
})
