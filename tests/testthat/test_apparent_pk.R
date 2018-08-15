context("apparent_pk")

test_that("apparent_pk_co2_hco3 returns correct number", {
  expect_equal(apparent_pk_co2_hco3(38, 7.4), 6.086, tolerance=0.000001)
  expect_equal(apparent_pk_co2_hco3(37, 7.35), 6.0928895)
})

test_that("apparent_pk_co2_hco3 is vectorised", {
  temp_vector <- c(38, 37)
  ph_vector <- c(7.4, 7.35)
  expect_equal(apparent_pk_co2_hco3(temp_vector, ph_vector), c(6.086, 6.0928895), tolerance=0.000001)
})
