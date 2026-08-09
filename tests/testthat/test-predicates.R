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

# is.numeric() -----------------------------------------------------------------

test_that("is.numeric() returns TRUE for a vecvec of numeric slots", {
  x <- vecvec(1:5, c(1.5, 2.5))
  expect_true(is.numeric(x))
})

test_that("is.numeric() returns FALSE for a vecvec of character slots", {
  x <- vecvec(letters)
  expect_false(is.numeric(x))
})

test_that("is.numeric() returns FALSE when only some slots are numeric", {
  x <- vecvec(1:5, letters[1:3])
  expect_false(is.numeric(x))
})

test_that("is.numeric() returns TRUE for a zero-length vecvec", {
  expect_true(is.numeric(vecvec()))
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

# duplicated() ------------------------------------------------------------------

test_that("duplicated() returns all FALSE when no duplicates", {
  vv <- vecvec(1:5, 6:10)
  expect_identical(
    duplicated(vv),
    rep(FALSE, 10)
  )
})

test_that("duplicated() detects duplicates within the same sub-vector type", {
  vv <- c(vecvec(1:5), vecvec(3:7))
  plain <- c(1:5, 3:7)
  expect_identical(
    duplicated(vv),
    duplicated(plain)
  )
})

test_that("duplicated() detects duplicates across sub-vector types", {
  vv <- c(vecvec(1L, 2L, 3L), vecvec(3L))
  expect_identical(
    duplicated(vv),
    c(FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("duplicated() fromLast marks first occurrence as duplicate", {
  vv <- c(vecvec(1:5), vecvec(3:7))
  plain <- c(1:5, 3:7)
  expect_identical(
    duplicated(vv, fromLast = TRUE),
    duplicated(plain, fromLast = TRUE)
  )
})

test_that("duplicated() on a zero-length vecvec returns logical(0)", {
  expect_identical(duplicated(vecvec()), logical(0))
})

test_that("duplicated() on a single-element vecvec returns FALSE", {
  expect_identical(duplicated(vecvec(42L)), FALSE)
})

# anyDuplicated() --------------------------------------------------------------

test_that("anyDuplicated() returns 0L when no duplicates", {
  vv <- vecvec(1:10)
  expect_identical(anyDuplicated(vv), 0L)
})

test_that("anyDuplicated() returns index of first duplicate", {
  vv <- c(vecvec(1:10, 3, 1L))
  expect_identical(anyDuplicated(vv), 12L)
})

test_that("anyDuplicated() fromLast returns index of last duplicate", {
  vv <- c(vecvec(1:5, "a", 3:7))
  expect_identical(
    anyDuplicated(vv, fromLast = TRUE),
    5L
  )
})

test_that("anyDuplicated() on a zero-length vecvec returns 0L", {
  expect_identical(anyDuplicated(vecvec()), 0L)
})

test_that("anyDuplicated() on a single-element vecvec returns 0L", {
  expect_identical(anyDuplicated(vecvec(1L)), 0L)
})

# duplicated()/anyDuplicated()/unique() with record (vctrs_rcrd) slots ---------
#
# Slots holding a `vctrs_rcrd` (or any class whose `[` doesn't survive
# `unlist(recursive = FALSE)` reconstruction) used to be silently destructured
# into their fields instead of concatenated, producing NA (duplicated()) or a
# false "no duplicates" (anyDuplicated()). See R/predicates.R.

new_myrcrd <- function(a, b) {
  vctrs::new_rcrd(vctrs::vec_recycle_common(a = a, b = b), class = "myrcrd")
}

test_that("duplicated() detects duplicates within a single record slot", {
  rc <- new_myrcrd(c(1, 1, 2), 1)
  vv <- vecvec(rc)
  expect_identical(duplicated(vv), duplicated(rc))
  expect_identical(duplicated(vv), c(FALSE, TRUE, FALSE))
})

test_that("duplicated() detects duplicates across multiple record slots", {
  rc1 <- new_myrcrd(c(1, 1, 2), 1)
  rc2 <- new_myrcrd(c(2, 3), 1)
  vv <- vecvec(rc1, rc2)
  plain <- vctrs::vec_c(rc1, rc2)
  expect_identical(duplicated(vv), duplicated(plain))
  expect_identical(duplicated(vv), c(FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("anyDuplicated() finds the first duplicate position across record slots", {
  rc1 <- new_myrcrd(c(1, 1, 2), 1)
  rc2 <- new_myrcrd(c(2, 3), 1)
  vv <- vecvec(rc1, rc2)
  plain <- vctrs::vec_c(rc1, rc2)
  expect_identical(anyDuplicated(vv), which(duplicated(plain))[[1L]])
  expect_identical(anyDuplicated(vv), 2L)
})

test_that("anyDuplicated() returns 0L when no duplicates among record slots", {
  rc1 <- new_myrcrd(c(1, 2), 1)
  rc2 <- new_myrcrd(3, 1)
  vv <- vecvec(rc1, rc2)
  expect_identical(anyDuplicated(vv), 0L)
})

test_that("unique() returns correct records for a vecvec with record slots", {
  rc1 <- new_myrcrd(c(1, 1, 2), 1)
  rc2 <- new_myrcrd(c(2, 3), 1)
  vv <- vecvec(rc1, rc2)
  plain <- vctrs::vec_c(rc1, rc2)

  expect_identical(
    vctrs::vec_data(unvecvec(unique(vv))),
    vctrs::vec_data(plain[!duplicated(plain)])
  )
  expect_length(unique(vv), 3L)
})
