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
