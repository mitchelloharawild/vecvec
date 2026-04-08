test_that("is.na() returns FALSE for vecvec with no NAs", {
  x <- vecvec(c(1, 2, 3))
  expect_equal(is.na(x), c(FALSE, FALSE, FALSE))
})

test_that("is.na() detects NA in a vecvec (#8)", {
  x <- vecvec(c(1, NA, 3))
  expect_equal(is.na(x), c(FALSE, TRUE, FALSE))
})

test_that("is.na() detects NAs at the start and end of a vecvec", {
  x <- vecvec(c(NA, 2, NA))
  expect_equal(is.na(x), c(TRUE, FALSE, TRUE))
})

test_that("is.na() returns all TRUE for an all-NA vecvec", {
  x <- vecvec(c(NA, NA, NA))
  expect_equal(is.na(x), c(TRUE, TRUE, TRUE))
})

test_that("is.na() works on a length-1 NA vecvec", {
  x <- vecvec(NA_real_)
  expect_true(is.na(x))
})