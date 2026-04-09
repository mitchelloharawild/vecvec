
# Complex group: Re(), Im(), Mod(), Arg(), Conj()

# Helpers
make_complex_vv <- function() vecvec(1:5 + 0i, complex(real = 0, imaginary = 1:3))
make_real_vv    <- function() vecvec(1:5, 6:10)

# --- Re() -------------------------------------------------------------------

test_that("Re() on a purely real vecvec returns its values unchanged", {
  x <- 1:5
  expect_equal(Re(vecvec(x)), as.double(x))
})

test_that("Re() extracts real parts from a complex vecvec", {
  z <- c(1 + 2i, 3 + 4i, 5 + 6i)
  expect_equal(Re(vecvec(z)), Re(z))
})

test_that("Re() on a multi-segment vecvec matches base Re()", {
  vv <- make_complex_vv()
  expect_equal(Re(vv), Re(unvecvec(vv)))
})

test_that("Re() result has correct length", {
  vv <- make_complex_vv()
  expect_equal(length(Re(vv)), length(vv))
})

# --- Im() -------------------------------------------------------------------

test_that("Im() on a purely real vecvec returns zeros", {
  expect_equal(Im(vecvec(1:5)), rep(0, 5))
})

test_that("Im() extracts imaginary parts from a complex vecvec", {
  z <- c(1 + 2i, 3 + 4i, 5 + 6i)
  expect_equal(Im(vecvec(z)), Im(z))
})

test_that("Im() on a multi-segment vecvec matches base Im()", {
  vv <- make_complex_vv()
  expect_equal(Im(vv), Im(unvecvec(vv)))
})

test_that("Im() result has correct length", {
  vv <- make_complex_vv()
  expect_equal(length(Im(vv)), length(vv))
})

# --- Mod() ------------------------------------------------------------------

test_that("Mod() on a purely real positive vecvec equals the values", {
  x <- 1:5
  expect_equal(Mod(vecvec(x)), as.double(x))
})

test_that("Mod() on a purely real negative vecvec returns absolute values", {
  x <- -5:-1
  expect_equal(Mod(vecvec(x)), abs(as.double(x)))
})

test_that("Mod() on a complex vecvec matches base Mod()", {
  z <- c(3 + 4i, 5 + 12i)
  expect_equal(Mod(vecvec(z)), Mod(z))
})

test_that("Mod() on a multi-segment complex vecvec matches base Mod()", {
  vv <- make_complex_vv()
  expect_equal(Mod(vv), Mod(unvecvec(vv)))
})

# --- Arg() ------------------------------------------------------------------

test_that("Arg() on a positive real vecvec returns zeros", {
  expect_equal(Arg(vecvec(1:5)), rep(0, 5))
})

test_that("Arg() on a purely imaginary vecvec returns pi/2", {
  z <- c(0 + 1i, 0 + 2i)
  expect_equal(Arg(vecvec(z)), Arg(z))
})

test_that("Arg() on a complex vecvec matches base Arg()", {
  z <- c(1 + 1i, -1 + 1i, -1 - 1i, 1 - 1i)
  expect_equal(Arg(vecvec(z)), Arg(z))
})

test_that("Arg() on a multi-segment vecvec matches base Arg()", {
  vv <- make_complex_vv()
  expect_equal(Arg(vv), Arg(unvecvec(vv)))
})

# --- Conj() -----------------------------------------------------------------

test_that("Conj() on a purely real vecvec returns it unchanged (as complex)", {
  z <- c(1 + 0i, 2 + 0i, 3 + 0i)
  expect_equal(Conj(vecvec(z)), Conj(z))
})

test_that("Conj() negates imaginary parts", {
  z <- c(1 + 2i, 3 + 4i, 5 + 6i)
  expect_equal(Conj(vecvec(z)), Conj(z))
})

test_that("Conj() on a multi-segment complex vecvec matches base Conj()", {
  vv <- make_complex_vv()
  expect_equal(Conj(vv), Conj(unvecvec(vv)))
})

test_that("Conj() result has correct length", {
  z <- c(1 + 2i, 3 + 4i)
  expect_equal(length(Conj(vecvec(z))), length(z))
})

test_that("Conj() applied twice returns the original values", {
  z <- c(1 + 2i, 3 - 4i, 0 + 5i)
  expect_equal(Conj(Conj(vecvec(z))), z)
})
