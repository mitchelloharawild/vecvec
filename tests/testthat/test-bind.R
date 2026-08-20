# Helpers ------------------------------------------------------------------

vv_int  <- function() vecvec(1:3)
vv_int2 <- function() vecvec(4:6)
vv_chr  <- function() vecvec(letters[1:3])

# cbind ----------------------------------------------------------------------

test_that("cbind() of two plain vecvecs matches base cbind() element order", {
  r <- cbind(vv_int(), vv_int2())
  expect_true(is_vecvec(r))
  expect_equal(dim(r), c(3L, 2L))
  expect_equal(as.integer(unvecvec(r)), as.integer(cbind(1:3, 4:6)))
})

test_that("cbind() preserves mixed element types", {
  r <- cbind(vv_chr(), vv_int())
  expect_equal(dim(r), c(3L, 2L))
  expect_equal(r[[1L]], "a")
  expect_equal(r[[4L]], 1L)
})

test_that("cbind() coerces a plain (non-vecvec) argument", {
  r <- cbind(vv_int(), 4:6)
  expect_true(is_vecvec(r))
  expect_equal(as.integer(unvecvec(r)), as.integer(cbind(1:3, 4:6)))
})

test_that("cbind() names columns from argument names", {
  r <- cbind(a = vv_int(), b = vv_int2())
  expect_equal(dimnames(r), list(NULL, c("a", "b")))
})

test_that("cbind() concatenates existing row/col names", {
  x <- array(vv_int2(), dim = c(3L, 1L))
  dimnames(x) <- list(c("r1", "r2", "r3"), "c1")
  y <- array(vv_int(), dim = c(3L, 1L))
  dimnames(y) <- list(c("r1", "r2", "r3"), "c2")
  r <- cbind(x, y)
  expect_equal(dimnames(r), list(c("r1", "r2", "r3"), c("c1", "c2")))
})

test_that("cbind() of mismatched row counts errors", {
  expect_error(cbind(vecvec(1:3), vecvec(1:4)))
})

# rbind ----------------------------------------------------------------------

test_that("rbind() of two plain vecvecs matches base rbind() element order", {
  r <- rbind(vv_int(), vv_int2())
  expect_true(is_vecvec(r))
  expect_equal(dim(r), c(2L, 3L))
  expect_equal(as.integer(unvecvec(r)), as.integer(rbind(1:3, 4:6)))
})

test_that("rbind() names rows from argument names", {
  r <- rbind(a = vv_int(), b = vv_int2())
  expect_equal(dimnames(r), list(c("a", "b"), NULL))
})

# vecvec_abind -----------------------------------------------------------------

test_that("vecvec_abind() with along = 2 matches cbind()", {
  expect_equal(vecvec_abind(vv_int(), vv_int2(), along = 2L), cbind(vv_int(), vv_int2()))
})

test_that("vecvec_abind() with along = 1 matches rbind()", {
  expect_equal(vecvec_abind(vv_int(), vv_int2()), rbind(vv_int(), vv_int2()))
})

test_that("vecvec_abind() stacks two matrices into a 3D array", {
  m1 <- array(vecvec(1:6), dim = c(2L, 3L))
  m2 <- array(vecvec(7:12), dim = c(2L, 3L))
  r <- vecvec_abind(m1, m2, along = 3L)
  expect_equal(dim(r), c(2L, 3L, 2L))
  expect_equal(as.integer(unvecvec(r)), 1:12)
})

test_that("vecvec_abind() with a single argument returns it reshaped, unchanged in content", {
  r <- vecvec_abind(vv_int(), along = 2L)
  expect_equal(dim(r), c(3L, 1L))
  expect_equal(as.integer(unvecvec(r)), 1:3)
})

test_that("vecvec_abind() with no arguments returns an empty vecvec", {
  r <- vecvec_abind()
  expect_true(is_vecvec(r))
  expect_equal(length(r), 0L)
})

test_that("vecvec_abind() errors when non-along dimensions differ", {
  expect_error(vecvec_abind(vecvec(1:4), vecvec(1:6), along = 1L))
})
