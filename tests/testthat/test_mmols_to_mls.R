context("mmols_dl_to_mls_dl")

test_that("mmols_dl_to_mls_dl returns correct number", {
  expect_equal(mmols_dl_to_mls_dl(25), 61.973995, tolerance=0.0000001)
})

test_that("mmols_dl_to_mls_dl is vectorised", {
  mmols_dl_vector <- c(25, 25)
  expect_equal(mmols_dl_to_mls_dl(mmols_dl_vector), c(61.973995, 61.973995), tolerance=0.0000001)
})
