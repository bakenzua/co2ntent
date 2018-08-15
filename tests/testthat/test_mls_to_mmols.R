context("mls_dl_to_mmols_dl")

test_that("mls_dl_to_mmols_dl returns correct number", {
  expect_equal(mls_dl_to_mmols_dl(62), 25.010490, tolerance=0.0000001)
})

test_that("mls_dl_to_mmols_dl is vectorised", {
  mmols_dl_vector <- c(62, 62)
  expect_equal(mls_dl_to_mmols_dl(mmols_dl_vector), c(25.010490, 25.010490), tolerance=0.0000001)
})
