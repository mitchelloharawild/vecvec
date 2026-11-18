# The base vector of a vecvec resolves to the stored values (not the index) so
# base R / C code that reads the object directly gets the right numbers.

test_that("the base vector is a double that resolves to values, not indices", {
  x <- vecvec(c(10.5, 20.25, 31))[c(3, 1, 2)]
  expect_false(is.integer(x))
  expect_equal(as.vector(unclass(x)), c(31, 10.5, 20.25))
})

test_that("non-numeric slots resolve to NA in the base vector", {
  x <- vecvec(1:2, c("a", "b"))
  expect_equal(as.vector(unclass(x)), c(1, 2, NA, NA))
})

test_that("lm() uses vecvec values rather than indices", {
  vals <- c(10.5, 20.25, 31, 44, 52.5, 61, 7, 8.5)
  idx <- c(3, 1, 2, 5, 4, 6, 8, 7)
  z <- vals[idx]
  y <- 2 * z + c(.3, -.2, .1, .5, -.4, .2, 0, .1)
  d <- data.frame(y = y)
  d$x <- vecvec(vals)[idx]
  expect_equal(coef(lm(y ~ x, d)), coef(lm(y ~ z)), ignore_attr = TRUE)
})

test_that("data.matrix() and prcomp() use vecvec values", {
  vals <- c(10.5, 20.25, 31, 44, 52.5, 61, 7, 8.5)
  idx <- c(3, 1, 2, 5, 4, 6, 8, 7)
  d <- data.frame(a = vecvec(vals)[idx], b = vecvec(vals)[rev(idx)])
  p <- data.frame(a = vals[idx], b = vals[rev(idx)])
  expect_equal(data.matrix(d), data.matrix(p), ignore_attr = TRUE)
  expect_equal(prcomp(d)$sdev, prcomp(p)$sdev)
})

test_that("values stay correct after changing the stored vectors", {
  x <- vecvec(c(1, 2, 3))
  x[2] <- 20
  expect_equal(as.vector(unclass(x)), c(1, 20, 3))
  y <- vecvec(c(1, 2, 3))
  y@x <- list(c(5, 6, 7))
  expect_equal(as.vector(unclass(y)), c(5, 6, 7))
})

test_that("the index survives serialization", {
  x <- vecvec(1:3, c(4.5, 5.5))[c(5, 1, 4)]
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f))
  saveRDS(x, f)
  y <- readRDS(f)
  expect_identical(vv_index(y), vv_index(x))
  expect_equal(unvecvec(y), unvecvec(x))
})

test_that("classed numeric slots resolve through as.double()", {
  registerS3method(
    "as.double", "vv_test_cyclical",
    function(x, ...) as.double(unclass(x)) + 1
  )
  cyc <- structure(c(0L, 1L, 2L), class = "vv_test_cyclical")
  # Index 3, 1, 2 into the slot: raw storage would give 2, 0, 1
  expect_equal(as.vector(vv_alt(c(3L, 1L, 2L), list(cyc))), c(3, 1, 2))
  # Unclassed slots are still read directly
  y <- vecvec(c(5, 6), 1:2)
  expect_equal(as.vector(unclass(y)), c(5, 6, 1, 2))
})
