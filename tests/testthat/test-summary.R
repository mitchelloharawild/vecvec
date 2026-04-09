
# Summary group: all(), any(), sum(), prod(), min(), max(), range()

# --- all() ------------------------------------------------------------------

test_that("all() returns TRUE when all elements are TRUE", {
  expect_true(all(vecvec(c(TRUE, TRUE), c(TRUE, TRUE))))
})

test_that("all() returns FALSE when any element is FALSE", {
  expect_false(all(vecvec(c(TRUE, FALSE), c(TRUE, TRUE))))
})

test_that("all() handles a single-segment vecvec", {
  expect_true(all(vecvec(c(TRUE, TRUE, TRUE))))
  expect_false(all(vecvec(c(TRUE, FALSE, TRUE))))
})

test_that("all() respects na.rm", {
  expect_true(all(vecvec(c(TRUE, NA), c(TRUE)), na.rm = TRUE))
  expect_false(all(vecvec(c(TRUE, FALSE), c(NA)), na.rm = TRUE))
  expect_identical(all(vecvec(c(TRUE, NA))), NA)
})

test_that("all() works across multiple vecvec arguments", {
  expect_true(all(vecvec(c(TRUE, TRUE)), vecvec(c(TRUE, TRUE))))
  expect_false(all(vecvec(c(TRUE, TRUE)), vecvec(c(TRUE, FALSE))))
})

test_that("all() works with mixed vecvec and plain vector arguments", {
  expect_true(all(vecvec(c(TRUE, TRUE)), c(TRUE, TRUE)))
  expect_false(all(vecvec(c(TRUE, TRUE)), c(TRUE, FALSE)))
})

# --- any() ------------------------------------------------------------------

test_that("any() returns TRUE when at least one element is TRUE", {
  expect_true(any(vecvec(c(FALSE, TRUE), c(FALSE))))
})

test_that("any() returns FALSE when all elements are FALSE", {
  expect_false(any(vecvec(c(FALSE, FALSE), c(FALSE, FALSE))))
})

test_that("any() respects na.rm", {
  expect_false(any(vecvec(c(FALSE, NA)), na.rm = TRUE))
  expect_identical(any(vecvec(c(FALSE, NA))), NA)
})

test_that("any() works across multiple vecvec arguments", {
  expect_false(any(vecvec(c(FALSE, FALSE)), vecvec(c(FALSE, FALSE))))
  expect_true(any(vecvec(c(FALSE, FALSE)), vecvec(c(FALSE, TRUE))))
})

# --- sum() ------------------------------------------------------------------

test_that("sum() of integer vecvec matches base sum()", {
  x <- 1:10
  expect_equal(sum(vecvec(x)), sum(x))
})

test_that("sum() of double vecvec matches base sum()", {
  x <- c(1.1, 2.2, 3.3)
  expect_equal(sum(vecvec(x)), sum(x))
})

test_that("sum() across multiple segments matches base sum()", {
  x <- 1:5
  y <- 6:10
  expect_equal(sum(vecvec(x, y)), sum(c(x, y)))
})

test_that("sum() respects na.rm", {
  x <- c(1, NA, 3)
  expect_equal(sum(vecvec(x), na.rm = TRUE), sum(x, na.rm = TRUE))
  expect_identical(sum(vecvec(x)), NA_real_)
})

test_that("sum() works across multiple vecvec arguments", {
  expect_equal(
    sum(vecvec(1:5), vecvec(6:10)),
    sum(1:10)
  )
})

test_that("sum() works with mixed vecvec and plain vector arguments", {
  expect_equal(sum(vecvec(1:5), 6:10), sum(1:10))
})

# --- prod() -----------------------------------------------------------------

test_that("prod() of integer vecvec matches base prod()", {
  x <- 1:5
  expect_equal(prod(vecvec(x)), prod(x))
})

test_that("prod() across multiple segments matches base prod()", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  expect_equal(prod(vecvec(x, y)), prod(c(x, y)))
})

test_that("prod() respects na.rm", {
  x <- c(2, NA, 4)
  expect_equal(prod(vecvec(x), na.rm = TRUE), prod(x, na.rm = TRUE))
  expect_identical(prod(vecvec(x)), NA_real_)
})

# --- min() ------------------------------------------------------------------

test_that("min() of integer vecvec matches base min()", {
  expect_equal(min(vecvec(1:10)), 1L)
})

test_that("min() across multiple segments matches base min()", {
  expect_equal(min(vecvec(5:10, 1:4)), 1L)
})

test_that("min() of double vecvec matches base min()", {
  x <- c(3.5, 1.2, 4.8)
  expect_equal(min(vecvec(x)), min(x))
})

test_that("min() respects na.rm", {
  x <- c(5L, NA_integer_, 3L)
  expect_equal(min(vecvec(x), na.rm = TRUE), 3L)
  expect_identical(min(vecvec(x)), NA_integer_)
})

test_that("min() works across multiple vecvec arguments", {
  expect_equal(min(vecvec(5:10), vecvec(1:4)), 1L)
})

test_that("min() works with mixed vecvec and plain vector arguments", {
  expect_equal(min(vecvec(5:10), 1:4), 1L)
})

# --- max() ------------------------------------------------------------------

test_that("max() of integer vecvec matches base max()", {
  expect_equal(max(vecvec(1:10)), 10L)
})

test_that("max() across multiple segments matches base max()", {
  expect_equal(max(vecvec(1:5, 6:10)), 10L)
})

test_that("max() of double vecvec matches base max()", {
  x <- c(3.5, 1.2, 4.8)
  expect_equal(max(vecvec(x)), max(x))
})

test_that("max() respects na.rm", {
  x <- c(5L, NA_integer_, 3L)
  expect_equal(max(vecvec(x), na.rm = TRUE), 5L)
  expect_identical(max(vecvec(x)), NA_integer_)
})

test_that("max() works across multiple vecvec arguments", {
  expect_equal(max(vecvec(1:5), vecvec(6:10)), 10L)
})

test_that("max() works with mixed vecvec and plain vector arguments", {
  expect_equal(max(vecvec(1:5), 6:10), 10L)
})

# --- range() ----------------------------------------------------------------

test_that("range() of integer vecvec matches base range()", {
  expect_equal(range(vecvec(1:10)), c(1L, 10L))
})

test_that("range() across multiple segments matches base range()", {
  expect_equal(range(vecvec(5:10, 1:4)), c(1L, 10L))
})

test_that("range() of double vecvec matches base range()", {
  x <- c(3.5, 1.2, 4.8)
  expect_equal(range(vecvec(x)), range(x))
})

test_that("range() respects na.rm", {
  x <- c(3L, NA_integer_, 7L)
  expect_equal(range(vecvec(x), na.rm = TRUE), c(3L, 7L))
})
