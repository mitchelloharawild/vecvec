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

test_that("classed slots resolve through as.double() methods S7 registers as as.numeric", {
  # S7 registers `method(as.double, cls)` as `as.numeric.<cls>`, and a
  # namespace that binds `as.double` keeps its own `as.double` methods out of
  # base's S3 table, so internal dispatch on `as.double()` can't find them.
  registerS3method(
    "as.numeric", "vv_test_s7_double",
    function(x, ...) as.double(unclass(x)) + 1
  )
  cyc <- structure(c(0L, 1L, 2L), class = "vv_test_s7_double")
  x <- vecvec(cyc)[c(3, 1, 2)]
  expect_equal(as.vector(unclass(x)), c(3, 1, 2))
  expect_equal(as.numeric(x), c(3, 1, 2))
  z <- c(3, 1, 2)
  d <- data.frame(y = c(1.1, 2.3, 2.9), x = x)
  expect_equal(coef(lm(y ~ x, d)), coef(lm(d$y ~ z)), ignore_attr = TRUE)
})

# C code (e.g. tidyr's gather()) can copy the resolved values into a new plain
# double and copy the attributes over, which loses the ALTREP base.
copy_values <- function(x, f) {
  y <- f(as.vector(unclass(x)))
  attributes(y) <- attributes(x)
  y
}

test_that("a vecvec repeated by C code recovers its index", {
  x <- vecvec(c(1.5, 2.5), letters[1:2], c(1.5, 9))
  y <- copy_values(x, function(v) rep(v, 3))
  expect_identical(vv_index(y), rep(vv_index(x), 3))
  expect_equal(as.character(y), rep(as.character(x), 3))
  expect_equal(vctrs::vec_size(y), 18L)
  expect_true(vctrs::obj_is_vector(y))
})

test_that("a vecvec reordered by C code recovers its index from its values", {
  x <- vecvec(c(1.5, 2.5, 3.5), c(10L, 20L))
  y <- copy_values(x, function(v) v[c(5, 1, 1, 3)])
  expect_identical(vv_index(y), c(5L, 1L, 1L, 3L))
  expect_equal(unvecvec(y), unvecvec(x)[c(5, 1, 1, 3)])
})

test_that("a vecvec copied by C code errors when its values are ambiguous", {
  x <- vecvec(letters[1:3])
  y <- copy_values(x, function(v) v[c(2, 1)])
  expect_error(vv_index(y), "copied without its index")
})

# identical() only compares attributes and the base data, so vecvecs must be
# distinguishable through those alone.
test_that("identical() tells vecvecs with non-numeric slots apart", {
  r1 <- vctrs::new_rcrd(list(a = 1:2), class = "myrec")
  r2 <- vctrs::new_rcrd(list(a = 5:6), class = "myrec")
  expect_false(identical(vecvec("a"), vecvec("b")))
  expect_false(identical(vecvec(1, "a"), vecvec(1, "b")))
  expect_false(identical(vecvec(list(1, 2)), vecvec(list(3, 4))))
  expect_false(identical(vecvec(factor("a")), vecvec(factor("b"))))
  expect_false(identical(vecvec(r1), vecvec(r2)))
  expect_false(identical(vecvec(r1), vecvec(c("p", "q"))))
  expect_false(identical(vecvec(1:3), vecvec(4:6)))
})

test_that("identical() tells different indices into the same slots apart", {
  r1 <- vctrs::new_rcrd(list(a = 1:2), class = "myrec")
  expect_false(identical(vecvec(r1)[c(1, 1)], vecvec(r1)[c(2, 2)]))
  expect_false(identical(vecvec(c("a", "b"))[1:2], vecvec(c("a", "b"))[2:1]))
  expect_false(identical(vecvec(1:2)[1:2], vecvec(1:2)[2:1]))
})

test_that("identical() is TRUE for equal vecvecs", {
  r1 <- vctrs::new_rcrd(list(a = 1:2), class = "myrec")
  for (x in list(vecvec("a"), vecvec(1, "a"), vecvec(list(1, 2)),
                 vecvec(factor("a")), vecvec(r1), vecvec(r1)[c(2, 1)])) {
    expect_true(identical(x, x))
  }
  expect_true(identical(vecvec(1, "a"), vecvec(1, "a")))
  expect_true(identical(vecvec(r1)[c(2, 1)], vecvec(r1)[c(2, 1)]))
})

test_that("identical() follows changes to the stored vectors and index", {
  x <- vecvec("a", "b")
  y <- x
  y@x <- list(c("a", "z"))
  expect_false(identical(x, y))
  vv_index(y) <- 2:1
  expect_false(identical(vecvec(c("a", "z")), y))
  expect_identical(vecvec(c("a", "z"))[2:1], y)
  z <- x
  z[2] <- "q"
  expect_false(identical(x, z))
  expect_identical(z, vecvec("a", "q"))
})

test_that("serialization restores the mirror, and writes the data once", {
  r1 <- vctrs::new_rcrd(list(a = 1:2), class = "myrec")
  for (x in list(vecvec(1:3, c("a", "b"))[c(5, 1, 4)], vecvec(r1)[c(2, 2)])) {
    y <- unserialize(serialize(x, NULL))
    expect_identical(attr(y, "_vecvec"), attr(x, "_vecvec"))
    expect_identical(vv_index(y), vv_index(x))
    expect_identical(y@x, x@x)
  }
  # Not serialized twice
  big <- vecvec(runif(1e5))
  expect_lt(length(serialize(big, NULL)), 1.2 * (8 + 4) * 1e5)
  # Without the mirror (or with a wrapped base) the full state is still used
  w <- vecvec(c("a", "b"))
  attr(w, "_vecvec") <- NULL
  expect_equal(unvecvec(unserialize(serialize(w, NULL))), c("a", "b"))
  d <- vecvec(c("a", "b", "c", "d"))
  dim(d) <- c(2, 2)
  expect_identical(unserialize(serialize(d, NULL)), d)
})

test_that("the stored vectors of a copied vecvec are readable without its index", {
  x <- vecvec(letters[1:3])
  y <- copy_values(x, function(v) v[c(2, 1)])
  expect_identical(y@x, x@x)
})

test_that("a vecvec copied without its `_vecvec` attribute errors clearly", {
  x <- vecvec(c(1.5, 2.5))
  y <- as.vector(unclass(x))[1:2]
  attributes(y) <- attributes(x)[setdiff(names(attributes(x)), "_vecvec")]
  expect_error(vv_index(y), "copied without its `_vecvec` attribute")
})

test_that("serialization keeps the data of a vecvec given another's attributes", {
  # The mirror copied from `b` is stale, so `a` must not be restored from it.
  a <- vecvec(c(1, 2, 3))
  b <- vecvec(c(7, 8, 9))
  attributes(a) <- attributes(b)
  expect_equal(unvecvec(unserialize(serialize(a, NULL))), c(1, 2, 3))
  a <- vecvec(c(1, 2, 3))
  mostattributes(a) <- attributes(b)
  r <- unserialize(serialize(a, NULL))
  expect_equal(unvecvec(r), c(1, 2, 3))
  # The restored mirror is current again
  expect_identical(attr(r, "_vecvec")[[1]], vv_index(r))
})

# R's arithmetic reuses an unshared operand for its result, writing it through
# `REAL()` into the resolved-value cache of the base.
test_that("values written into the base survive copies and serialization", {
  x <- vecvec(c(1.5, 2.5, 3.5))
  y <- unclass(x) * 2
  expect_equal(as.vector(y), c(3, 5, 7))
  z <- y
  z[1] <- 0
  expect_equal(as.vector(z), c(0, 5, 7))
  expect_equal(as.vector(y), c(3, 5, 7))
  expect_equal(as.vector(unserialize(serialize(y, NULL))), c(3, 5, 7))
  # Reading through a writable pointer without writing changes nothing
  d <- data.frame(y = c(1, 3, 2), x = x)
  lm(y ~ x, d)
  expect_identical(vv_index(d$x), 1:3)
  expect_identical(vv_index(unserialize(serialize(d$x, NULL))), 1:3)
})

test_that("a vecvec whose base was written to recovers its index from the values", {
  x <- vecvec(c(1.5, 2.5, 3.5), c(10L, 20L))
  y <- unclass(x) * 1
  y[] <- y[c(5, 1, 1, 3, 2)]
  attributes(y) <- attributes(x)
  expect_identical(vv_index(y), c(5L, 1L, 1L, 3L, 2L))
})
