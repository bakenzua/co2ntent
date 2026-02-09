context("kelman_virtual_po2")

test_that("kelman_virtual_po2 returns correct number", {
  expect_equal(co2ntent:::kelman_virtual_po2(po2=10, temperature=37, ph=7.4, pco2=5.3329, pressure_units = "kPa"), 10, tolerance=0.000001)
})

test_that("kelman_virtual_po2 returns correct number for mmHg", {
  expect_equal(co2ntent:::kelman_virtual_po2(po2=75.006157584565642, temperature=37, ph=7.4, pco2=40, pressure_units = "mmHg"), 75, tolerance=0.001)
})

test_that("kelman_virtual_po2 returns is vectorised", {
  po2s <- c(10, 10, 10, 10, 10)
  expected <- c(10, 10, 10, 10, 10)
  expect_equal(co2ntent:::kelman_virtual_po2(po2=po2s, temperature=37, ph=7.4, pco2=5.3329, pressure_units = "kPa"), expected, tolerance=0.000001)
})

test_that("kelman_virtual_po2 doesn't return Inf if pco2 = 0", {
  expect_failure(expect_equal(co2ntent:::kelman_virtual_po2(po2=10, temperature=37, ph=7.4, pco2=0, pressure_units = "kPa"), Inf, tolerance=0.000001))

})

