context("kelman_virtual_po2")

test_that("kelman_virtual_po2 returns correct number", {
  expect_equal(kelman_virtual_po2(po2=10, temperature=37, ph=7.4, pco2=5.3329, inputs_are_kpa = TRUE), 10, tolerance=0.000001)
})

test_that("kelman_virtual_po2 returns correct number for mmHg", {
  expect_equal(kelman_virtual_po2(po2=75.006157584565642, temperature=37, ph=7.4, pco2=40, inputs_are_kpa = FALSE), 75, tolerance=0.001)
})

test_that("kelman_virtual_po2 returns is vectorised", {
  po2s <- c(10, 10, 10, 10, 10)
  expected <- c(10, 10, 10, 10, 10)
  expect_equal(kelman_virtual_po2(po2=po2s, temperature=37, ph=7.4, pco2=5.3329), expected, tolerance=0.000001)
})

test_that("kelman_virtual_po2 doesn't return Inf if pco2 = 0", {
  expect_failure(expect_equal(kelman_virtual_po2(po2=10, temperature=37, ph=7.4, pco2=0, inputs_are_kpa = TRUE, skip_range_check=TRUE), Inf, tolerance=0.000001))

})

