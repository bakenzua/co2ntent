context("kelman_po2_to_so2")

test_that("kelman_po2_to_so2 returns correct number", {
  expect_equal(kelman_po2_to_so2(10, 37, 7.4, 5.3329, inputs_are_kpa = TRUE), 0.950795, tolerance=0.000001)
})

test_that("kelman_po2_to_so2 returns correct number for mmHg", {
  expect_equal(kelman_po2_to_so2(75.006157584565642, 37, 7.4, 40, inputs_are_kpa = FALSE), 0.950795, tolerance=0.000001)
})

test_that("kelman_po2_to_so2 returns is vectorised", {
  po2s <- c(10, 10, 10, 10, 10)
  expected <- c(0.950795, 0.950795, 0.950795, 0.950795, 0.950795)
  expect_equal(kelman_po2_to_so2(po2s, 37, 7.4, 5.3329), expected, tolerance=0.000001)
})

test_that("kelman_po2_to_so2 returns is vectorised, multiple params", {
  po2s <- c(10, 5, 10, 5, 10)
  ts <- c(35, 37, 37, 37, 37)
  phs <- c(7.4, 7.4, 7.4, 7.4, 7.4)
  co2s <- c(5.3329, 5.3329, 5.3329, 5.3329, 5.3329)
  expected <- c(0.962480, 0.703525, 0.950795, 0.703525, 0.950795)
  expect_equal(kelman_po2_to_so2(po2s, ts, phs, co2s), expected, tolerance=0.000001)
})

