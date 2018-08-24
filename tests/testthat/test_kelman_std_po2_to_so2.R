context("kelman_std_po2_to_so2")

test_that("kelman_std_po2_to_so2 returns correct number", {
  expect_equal(kelman_std_po2_to_so2(10), 0.950795, tolerance=0.000001)
})

test_that("kelman_std_po2_to_so2 returns correct number for mmHg units", {
  expect_equal(kelman_std_po2_to_so2(10 / 0.133322, inputs_are_kpa = FALSE), 0.950795, tolerance=0.000001)
})
