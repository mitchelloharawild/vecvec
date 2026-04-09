# is.finite() ----------------------------------------------------------------

test_that("is.finite() returns TRUE for vecvec with all finite values", {
  x <- vecvec(c(1, 2, 3))
  expect_equal(is.finite(x), c(TRUE, TRUE, TRUE))
})

test_that("is.finite() returns FALSE for Inf in a vecvec", {
  x <- vecvec(c(1, Inf, 3))
  expect_equal(is.finite(x), c(TRUE, FALSE, TRUE))
})

test_that("is.finite() returns FALSE for -Inf in a vecvec", {
  x <- vecvec(c(-Inf, 2, 3))
  expect_equal(is.finite(x), c(FALSE, TRUE, TRUE))
})

test_that("is.finite() returns FALSE for NaN in a vecvec", {
  x <- vecvec(c(1, NaN, 3))
  expect_equal(is.finite(x), c(TRUE, FALSE, TRUE))
})

test_that("is.finite() returns FALSE for NA in a vecvec", {
  x <- vecvec(c(1, NA, 3))
  expect_equal(is.finite(x), c(TRUE, FALSE, TRUE))
})

test_that("is.finite() works on a length-1 finite vecvec", {
  x <- vecvec(1.5)
  expect_true(is.finite(x))
})

test_that("is.finite() works on a length-1 Inf vecvec", {
  x <- vecvec(Inf)
  expect_false(is.finite(x))
})

# is.infinite() --------------------------------------------------------------

test_that("is.infinite() returns FALSE for vecvec with no infinite values", {
  x <- vecvec(c(1, 2, 3))
  expect_equal(is.infinite(x), c(FALSE, FALSE, FALSE))
})

test_that("is.infinite() detects Inf in a vecvec", {
  x <- vecvec(c(1, Inf, 3))
  expect_equal(is.infinite(x), c(FALSE, TRUE, FALSE))
})

test_that("is.infinite() detects -Inf in a vecvec", {
  x <- vecvec(c(-Inf, 2, 3))
  expect_equal(is.infinite(x), c(TRUE, FALSE, FALSE))
})

test_that("is.infinite() detects Inf and -Inf at the start and end of a vecvec", {
  x <- vecvec(c(Inf, 2, -Inf))
  expect_equal(is.infinite(x), c(TRUE, FALSE, TRUE))
})

test_that("is.infinite() returns FALSE for NaN in a vecvec", {
  x <- vecvec(c(1, NaN, 3))
  expect_equal(is.infinite(x), c(FALSE, FALSE, FALSE))
})

test_that("is.infinite() returns FALSE for NA in a vecvec", {
  x <- vecvec(c(1, NA, 3))
  expect_equal(is.infinite(x), c(FALSE, FALSE, FALSE))
})

test_that("is.infinite() works on a length-1 Inf vecvec", {
  x <- vecvec(Inf)
  expect_true(is.infinite(x))
})

# is.nan() -------------------------------------------------------------------

test_that("is.nan() returns FALSE for vecvec with no NaN values", {
  x <- vecvec(c(1, 2, 3))
  expect_equal(is.nan(x), c(FALSE, FALSE, FALSE))
})

test_that("is.nan() detects NaN in a vecvec", {
  x <- vecvec(c(1, NaN, 3))
  expect_equal(is.nan(x), c(FALSE, TRUE, FALSE))
})

test_that("is.nan() detects NaN at the start and end of a vecvec", {
  x <- vecvec(c(NaN, 2, NaN))
  expect_equal(is.nan(x), c(TRUE, FALSE, TRUE))
})

test_that("is.nan() returns all TRUE for an all-NaN vecvec", {
  x <- vecvec(c(NaN, NaN, NaN))
  expect_equal(is.nan(x), c(TRUE, TRUE, TRUE))
})

test_that("is.nan() returns FALSE for NA (not NaN) in a vecvec", {
  x <- vecvec(c(1, NA, 3))
  expect_equal(is.nan(x), c(FALSE, FALSE, FALSE))
})

test_that("is.nan() returns FALSE for Inf in a vecvec", {
  x <- vecvec(c(1, Inf, 3))
  expect_equal(is.nan(x), c(FALSE, FALSE, FALSE))
})

test_that("is.nan() works on a length-1 NaN vecvec", {
  x <- vecvec(NaN)
  expect_true(is.nan(x))
})

# is.na() --------------------------------------------------------------------

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

test_that("is.na() returns TRUE for NaN (NaN is also NA)", {
  x <- vecvec(c(1, NaN, 3))
  expect_equal(is.na(x), c(FALSE, TRUE, FALSE))
})

# anyNA() ----------------------------------------------------------------------

test_that("anyNA() returns FALSE for vecvec with no NAs", {
  vv <- vecvec(c(1, 2, 3))
  expect_false(anyNA(vv))
})

test_that("anyNA() returns TRUE when NA is present in a vecvec", {
  vv <- vecvec(c(1, NA, 3))
  expect_true(anyNA(vv))
})

test_that("anyNA() returns TRUE when NA is at the start of a vecvec", {
  vv <- vecvec(c(NA, 2, 3))
  expect_true(anyNA(vv))
})

test_that("anyNA() returns TRUE when NA is at the end of a vecvec", {
  vv <- vecvec(c(1, 2, NA))
  expect_true(anyNA(vv))
})

test_that("anyNA() returns TRUE for an all-NA vecvec", {
  vv <- vecvec(c(NA, NA, NA))
  expect_true(anyNA(vv))
})

test_that("anyNA() returns TRUE for NaN (NaN is also NA)", {
  vv <- vecvec(c(1, NaN, 3))
  expect_true(anyNA(vv))
})

test_that("anyNA() returns FALSE for Inf in a vecvec", {
  vv <- vecvec(c(1, Inf, 3))
  expect_false(anyNA(vv))
})

test_that("anyNA() returns FALSE for -Inf in a vecvec", {
  vv <- vecvec(c(-Inf, 2, 3))
  expect_false(anyNA(vv))
})

test_that("anyNA() returns FALSE on a zero-length vecvec", {
  expect_false(anyNA(vecvec()))
})

test_that("anyNA() returns FALSE on a single-element finite vecvec", {
  expect_false(anyNA(vecvec(1.5)))
})

test_that("anyNA() returns TRUE on a single-element NA vecvec", {
  expect_true(anyNA(vecvec(NA_real_)))
})

test_that("anyNA() returns TRUE when NA appears across multiple sub-vectors", {
  vv <- c(vecvec(1:3), vecvec(c(4, NA, 6)))
  expect_true(anyNA(vv))
})

test_that("anyNA() returns FALSE when no NA across multiple sub-vectors", {
  vv <- c(vecvec(1:3), vecvec(4:6))
  expect_false(anyNA(vv))
})

test_that("anyNA() returns TRUE when NA is in the first of multiple sub-vectors", {
  vv <- c(vecvec(c(1, NA, 3)), vecvec(4:6))
  expect_true(anyNA(vv))
})
