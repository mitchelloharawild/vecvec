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

# unique() ---------------------------------------------------------------------

test_that("unique() returns unchanged vecvec when no duplicates", {
  vv <- vecvec(1:10)
  expect_identical(unique(vv), vv)
})

test_that("unique() removes duplicated elements", {
  vv <- c(vecvec(1:5), vecvec(3:7))
  expect_identical(
    as.integer(unique(vv)),
    1:7
  )
})

test_that("unique() on a zero-length vecvec returns zero-length vecvec", {
  vv <- vecvec()
  expect_identical(length(unique(vv)), 0L)
})

test_that("unique() on a single-element vecvec returns the same element", {
  vv <- vecvec(99L)
  expect_identical(unique(vv), vv)
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