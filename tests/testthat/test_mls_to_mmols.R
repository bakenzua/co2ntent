context("mls_dl_to_mmols_l")

test_that("mls_dl_to_mmols_l returns correct values for gases", {
  expect_equal(mls_dl_to_mmols_l(25 * 10 * 2.2263, gas = "co2"), 25, tolerance = 1e-7)
  expect_equal(mls_dl_to_mmols_l(25 * 10 * 2.2393, gas = "o2"), 25, tolerance = 1e-7)
  expect_equal(mls_dl_to_mmols_l(25 * 10 * 2.2414, gas = "ideal"), 25, tolerance = 1e-7)
})

test_that("mls_dl_to_mmols_l is vectorised", {
  mls_vec <- c(25 * 10 * 2.2263, 25 * 10 * 2.2263)
  expect_equal(mls_dl_to_mmols_l(mls_vec, gas = "co2"), c(25, 25), tolerance = 1e-7)
})

test_that("mls_dl_to_mmols_l preserves names and accepts molar_volume override", {
  x <- setNames(c(25 * 10 * 2.2414, 30 * 10 * 2.2414), c("a", "b"))
  out <- mls_dl_to_mmols_l(x, gas = "ideal")
  expect_named(out, c("a", "b"))
  expect_equal(out[["a"]], 25, tolerance = 1e-7)

  # override molar volume (dL/mmol)
  expect_equal(mls_dl_to_mmols_l(10, molar_volume = 0.5), 10 / (10 * 0.5), tolerance = 1e-7)
})