context("apparent_pk")

test_that("apparent_pk returns correct number", {
  expect_equal(apparent_pk(38, 7.4), 6.086)
})
