context("mls_dl_to_mmols_l")

test_that("mls_dl_to_mmols_l returns correct number", {
  expect_equal(mls_dl_to_mmols_l(62), 27.299571, tolerance=0.0000001)
})

test_that("mls_dl_to_mmols_l is vectorised", {
  mmols_l_vector <- c(62, 62)
  expect_equal(mls_dl_to_mmols_l(mmols_l_vector), c(27.299571, 27.299571), tolerance=0.0000001)
})
