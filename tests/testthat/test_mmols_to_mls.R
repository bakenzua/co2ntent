context("mmols_l_to_mls_dl")

test_that("mmols_l_to_mls_dl returns correct values for gases", {
  expect_equal(mmols_l_to_mls_dl(25, gas = "co2"), 25 * 10 * 2.2263, tolerance = 1e-7)
  expect_equal(mmols_l_to_mls_dl(25, gas = "o2"), 25 * 10 * 2.2393, tolerance = 1e-7)
  expect_equal(mmols_l_to_mls_dl(25, gas = "ideal"), 25 * 10 * 2.2414, tolerance = 1e-7)
})

test_that("mmols_l_to_mls_dl is vectorised", {
  mmols_l_vector <- c(25, 25)
  expect_equal(mmols_l_to_mls_dl(mmols_l_vector, gas = "co2"), c(25, 25) * 10 * 2.2263, tolerance = 1e-7)
})

test_that("mmols_l_to_mls_dl preserves names and accepts molar_volume override", {
  x <- setNames(c(25, 30), c("a", "b"))
  out <- mmols_l_to_mls_dl(x, gas = "ideal")
  expect_named(out, c("a", "b"))
  expect_equal(out[["a"]], 25 * 10 * 2.2414, tolerance = 1e-3)

  # override molar volume (dL/mmol)
  expect_equal(mmols_l_to_mls_dl(2, molar_volume = 0.5), 2 * 10 * 0.5, tolerance = 1e-7)
})
