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

# all.equal() -------------------------------------------------------------

test_that("all.equal() is TRUE for identical vecvecs", {
  vv <- vecvec(1:3, letters[1:3])
  expect_true(all.equal(vv, vecvec(1:3, letters[1:3])))
})

test_that("all.equal() is TRUE across different internal representations", {
  vv1 <- vecvec(1:3, letters[1:3])
  vv2 <- c(vecvec(1:3), vecvec(letters[1:3]))
  expect_true(all.equal(vv1, vv2))
})

test_that("all.equal() reports a message (not an error) when slots differ", {
  vv1 <- vecvec(1:3, letters[1:3])
  vv2 <- vecvec(1:3, letters[4:6])
  res <- all.equal(vv1, vv2)
  expect_type(res, "character")
})

test_that("all.equal() reports length mismatches", {
  expect_match(
    all.equal(vecvec(1:3), vecvec(1:2)),
    "Lengths"
  )
})

test_that("all.equal() is TRUE for empty vecvecs", {
  expect_true(all.equal(vecvec(), vecvec()))
})

test_that("all.equal() compares vecvec against a plain vector", {
  expect_true(all.equal(vecvec(1:3), 1:3))
  expect_type(all.equal(vecvec(1:3), c(1L, 2L, 5L)), "character")
})

test_that("all.equal() detects NA mismatches", {
  vv1 <- vecvec(c(1, 2, 3))
  vv2 <- vv1[c(1, 2, NA)]
  expect_match(all.equal(vv1, vv2), "missing")
})
