context("mmols_l_to_mls_dl")

test_that("mmols_l_to_mls_dl returns correct number", {
  expect_equal(mmols_l_to_mls_dl(25), 56.77745, tolerance=0.0000001)
})

test_that("mmols_l_to_mls_dl is vectorised", {
  mmols_l_vector <- c(25, 25)
  expect_equal(mmols_l_to_mls_dl(mmols_l_vector), c(56.77745, 56.77745), tolerance=0.0000001)
})
