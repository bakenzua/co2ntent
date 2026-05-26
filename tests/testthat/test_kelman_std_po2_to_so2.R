context("kelman_std_po2_to_so2")

test_that("kelman_std_po2_to_so2 returns correct number", {
  expect_equal(co2ntent:::kelman_std_po2_to_so2(10, po2_units = "kPa"), 0.950795, tolerance=0.000001)
})

test_that("kelman_std_po2_to_so2 returns correct number for mmHg units", {
  expect_equal(co2ntent:::kelman_std_po2_to_so2(10 / 0.133322, po2_units = "mmHg"), 0.950795, tolerance=0.000001)
})
