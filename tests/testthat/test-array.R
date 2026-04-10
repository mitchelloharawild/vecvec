# Helpers ------------------------------------------------------------------

vv_chr  <- function() vecvec(letters)
vv_dbl  <- function() vecvec(as.double(1:26))
vv_int  <- function() vecvec(1:26)
vv_date <- function() vecvec(Sys.Date() + 0:25)
vv_mix  <- function() vecvec(letters, as.double(1:26), 1:26, Sys.Date() + 0:25)

# dim<- / array construction -----------------------------------------------

test_that("dim<- sets dim attribute on vecvec", {
  x <- vv_chr()
  dim(x) <- c(2L, 13L)
  expect_equal(dim(x), c(2L, 13L))
  expect_true(is_vecvec(x))
})

test_that("array() wraps a single-type vecvec preserving class and dim", {
  x <- array(vv_int(), dim = c(2L, 13L))
  expect_equal(dim(x), c(2L, 13L))
  expect_true(is_vecvec(x))
})

test_that("array() wraps a mixed vecvec into a 2D array", {
  x <- array(vv_mix(), dim = c(8L, 13L))
  expect_equal(dim(x), c(8L, 13L))
  expect_true(is_vecvec(x))
})

test_that("array() wraps a mixed vecvec into a 3D array", {
  x <- array(vv_mix(), dim = c(8L, 3L, 4L))
  expect_equal(dim(x), c(8L, 3L, 4L))
  expect_true(is_vecvec(x))
})

test_that("dim<- round-trips: setting then reading dim is consistent", {
  x <- vv_dbl()
  dim(x) <- c(2L, 13L)
  expect_equal(dim(x), c(2L, 13L))
  dim(x) <- NULL
  expect_null(dim(x))
})

test_that("length is unchanged after dim<- assignment", {
  x <- vv_chr()
  dim(x) <- c(2L, 13L)
  expect_equal(length(x), 26L)
})

test_that("is_vecvec is TRUE for dim-annotated vecvec", {
  x <- vv_mix()
  dim(x) <- c(8L, 13L)
  expect_true(is_vecvec(x))
})

test_that("dim<- with a single dimension (vector form) is valid", {
  x <- vv_int()
  dim(x) <- 26L
  expect_equal(dim(x), 26L)
  expect_true(is_vecvec(x))
})

# dimnames<- ---------------------------------------------------------------

test_that("dimnames<- assigns row and column names", {
  x <- array(vv_int(), dim = c(2L, 13L))
  rn <- paste0("r", 1:2)
  cn <- paste0("c", 1:13)
  dimnames(x) <- list(rn, cn)
  expect_equal(dimnames(x), list(rn, cn))
})

test_that("dimnames<- with NULL clears names", {
  x <- array(vv_int(), dim = c(2L, 13L))
  dimnames(x) <- list(paste0("r", 1:2), paste0("c", 1:13))
  dimnames(x) <- NULL
  expect_null(dimnames(x))
})

test_that("dimnames<- preserves vecvec class and length", {
  x <- array(vv_int(), dim = c(2L, 13L))
  dimnames(x) <- list(paste0("r", 1:2), paste0("c", 1:13))
  expect_true(is_vecvec(x))
  expect_equal(length(x), 26L)
})

test_that("dimnames<- works for 3D array", {
  x <- array(vv_int(), dim = c(2L, 3L, 4L))
  dn <- list(
    paste0("r", 1:2),
    paste0("c", 1:3),
    paste0("z", 1:4)
  )
  dimnames(x) <- dn
  expect_equal(dimnames(x), dn)
  expect_true(is_vecvec(x))
})

# as.matrix ----------------------------------------------------------------

test_that("as.matrix on a character vecvec gives a column matrix", {
  m <- as.matrix(vv_chr())
  expect_true(is_vecvec(m))
  expect_equal(dim(m), c(26L, 1L))
})

test_that("as.matrix on a double vecvec gives a column matrix", {
  m <- as.matrix(vv_dbl())
  expect_true(is_vecvec(m))
  expect_equal(dim(m), c(26L, 1L))
})

test_that("as.matrix preserves element values via [[", {
  m <- as.matrix(vv_chr())
  expect_equal(m[[1L]], "a")
  expect_equal(m[[26L]], "z")
})

test_that("as.matrix on a mixed vecvec produces correct dim", {
  m <- as.matrix(vv_mix())
  expect_true(is_vecvec(m))
  expect_equal(dim(m), c(104L, 1L))
})

test_that("as.matrix preserves Date elements via [[", {
  m <- as.matrix(vv_date())
  expect_s3_class(m[[1L]], "Date")
})

# t (transpose) ------------------------------------------------------------

test_that("t() transposes a column matrix to a row matrix", {
  m  <- as.matrix(vv_chr())
  mt <- t(m)
  expect_equal(dim(mt), c(1L, 26L))
  expect_true(is_vecvec(mt))
})

test_that("t() on a 2-row integer matrix transposes dim correctly", {
  x  <- array(vv_int(), dim = c(2L, 13L))
  xt <- t(x)
  expect_equal(dim(xt), c(13L, 2L))
  expect_true(is_vecvec(xt))
})

test_that("t() preserves element values in transposed positions (via format)", {
  x  <- array(vv_int(), dim = c(2L, 13L))
  xt <- t(x)
  f  <- format(xt)
  # column-major: original [1,1]=1, [2,1]=2; after t: [1,1]=1, [1,2]=2
  expect_equal(trimws(f[1L, 1L]), "1")
  expect_equal(trimws(f[1L, 2L]), "2")
  expect_equal(trimws(f[2L, 1L]), "3")
})

test_that("t(t(x)) round-trips dim to original", {
  x <- array(vv_int(), dim = c(2L, 13L))
  expect_equal(dim(t(t(x))), dim(x))
})

# flat [[]] indexing on dim-annotated vecvec --------------------------------

test_that("[[i]] on a dim-annotated single-type vecvec returns scalar", {
  x <- array(vv_int(), dim = c(2L, 13L))
  expect_equal(x[[1L]], 1L)
  expect_equal(x[[26L]], 26L)
})

test_that("[[i]] on a dim-annotated mixed vecvec returns correct type", {
  x <- array(vv_mix(), dim = c(8L, 13L))
  expect_equal(x[[1L]],  "a")
  expect_equal(x[[26L]], "z")
  expect_equal(x[[27L]], 1)         # first double
  expect_s3_class(x[[79L]], "Date") # first Date
})

test_that("[[i]] on a 1x1 array returns the single element", {
  x <- array(vecvec(42L), dim = c(1L, 1L))
  expect_equal(x[[1L]], 42L)
})

# [i] flat indexing preserves class and dim --------------------------------

# [, j] column subset (note: [i, ] and [i, j] currently error) -----------

test_that("[, j] on a dim-annotated vecvec returns a vecvec", {
  x <- array(vv_int(), dim = c(2L, 13L))
  col1 <- x[, 1L]
  expect_true(is_vecvec(col1))
})

test_that("[i, ] on a dim-annotated vecvec returns the correct row as a vecvec", {
  x <- array(vv_int(), dim = c(2L, 13L))
  row1 <- x[1L, ]
  expect_true(is_vecvec(row1))
  expect_equal(as.integer(row1), seq(1L, 25L, by = 2L))
})

test_that("[i, j] on a dim-annotated vecvec returns the correct scalar element", {
  x <- array(vv_int(), dim = c(2L, 13L))
  expect_equal(x[1L, 1L], vecvec(1L))
  expect_equal(x[2L, 1L], vecvec(2L))
  expect_equal(x[1L, 2L], vecvec(3L))
})

# format -------------------------------------------------------------------

test_that("format() on a 2D dim-annotated vecvec returns a character matrix", {
  x <- array(vv_int(), dim = c(2L, 13L))
  f <- format(x)
  expect_true(is.character(f))
  expect_equal(dim(f), c(2L, 13L))
})

test_that("format() on 3D mixed vecvec array returns a character array", {
  x <- array(vv_mix(), dim = c(8L, 3L, 4L))
  f <- format(x)
  expect_true(is.character(f))
  expect_equal(dim(f), c(8L, 3L, 4L))
})

test_that("format() values match underlying elements (character slot)", {
  x <- array(vv_chr(), dim = c(2L, 13L))
  f <- format(x)
  expect_equal(trimws(f[1L, 1L]), "a")
  expect_equal(trimws(f[2L, 1L]), "b")
  expect_equal(trimws(f[1L, 2L]), "c")
})

# as.vector ----------------------------------------------------------------

test_that("as.vector on a dim-annotated vecvec retains the dim attribute", {
  # Current behaviour: as.vector is identity for vecvec (dim preserved)
  x <- array(vv_chr(), dim = c(2L, 13L))
  v <- as.vector(x)
  expect_true(is_vecvec(v))
  expect_equal(length(v), 26L)
})

# as.character / as.double / as.integer coercion --------------------------

test_that("as.character on a dim-annotated character vecvec returns letters", {
  x  <- array(vv_chr(), dim = c(2L, 13L))
  ch <- as.character(x)
  expect_equal(ch, letters)
})

test_that("as.integer on a dim-annotated integer vecvec returns 1:26", {
  x <- array(vv_int(), dim = c(2L, 13L))
  expect_equal(as.integer(x), 1:26)
})

test_that("as.double on a dim-annotated double vecvec returns correct values", {
  x <- array(vv_dbl(), dim = c(2L, 13L))
  expect_equal(as.double(x), as.double(1:26))
})

# Edge cases ---------------------------------------------------------------

test_that("zero-length vecvec with dim c(0, 0) is valid", {
  x <- vecvec()
  dim(x) <- c(0L, 0L)
  expect_equal(dim(x), c(0L, 0L))
  expect_equal(length(x), 0L)
  expect_true(is_vecvec(x))
})

test_that("1x1 array of vecvec has correct dim and element", {
  x <- array(vecvec(42L), dim = c(1L, 1L))
  expect_equal(dim(x), c(1L, 1L))
  expect_true(is_vecvec(x))
  expect_equal(x[[1L]], 42L)
})

test_that("3D array of mixed vecvec: Date elements accessible via [[", {
  x <- array(vv_mix(), dim = c(8L, 3L, 4L))
  # vv_mix: 26 chars + 26 doubles + 26 ints + 26 dates = 104 source elements;
  # array dim is 8×3×4 = 96 slots, so Date slot (starts at index 79) fills positions 79–96.
  expect_s3_class(x[[79L]], "Date")
  expect_s3_class(x[[96L]], "Date")
})
