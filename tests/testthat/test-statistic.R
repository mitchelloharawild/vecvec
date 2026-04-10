
# Statistics: mean(), weighted.mean(), median(), var(), quantile(), cov(), cor()

# --- mean() -----------------------------------------------------------------

test_that("mean() of a single-segment double vecvec matches base mean()", {
  x <- c(1.5, 2.5, 3.5, 4.5)
  expect_equal(mean(vecvec(x)), mean(x))
})

test_that("mean() of a multi-segment vecvec matches base mean()", {
  x <- 1:5
  y <- 6:10
  expect_equal(mean(vecvec(x, y)), mean(c(x, y)))
})

test_that("mean() of an integer vecvec returns a double", {
  expect_equal(mean(vecvec(1L, 2L, 3L)), 2)
})

test_that("mean() with NA returns NA by default", {
  x <- c(1, NA, 3)
  expect_identical(mean(vecvec(x)), NA_real_)
})

test_that("mean() respects na.rm = TRUE", {
  x <- c(1, NA, 3)
  expect_equal(mean(vecvec(x), na.rm = TRUE), mean(x, na.rm = TRUE))
})

# --- weighted.mean() --------------------------------------------------------

test_that("weighted.mean() matches base weighted.mean() for equal weights", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(1, 1, 1, 1, 1)
  expect_equal(weighted.mean(vecvec(x), w = w), weighted.mean(x, w = w))
})

test_that("weighted.mean() matches base weighted.mean() for unequal weights", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
  expect_equal(weighted.mean(vecvec(x), w = w), weighted.mean(x, w = w))
})

test_that("weighted.mean() of a multi-segment vecvec matches base", {
  x <- c(1.0, 2.0, 3.0)
  y <- c(4.0, 5.0)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
  expect_equal(
    weighted.mean(vecvec(x, y), w = w),
    weighted.mean(c(x, y), w = w)
  )
})

test_that("weighted.mean() respects na.rm = TRUE", {
  x <- c(1, NA, 3)
  w <- c(1, 1, 1)
  expect_equal(
    weighted.mean(vecvec(x), w = w, na.rm = TRUE),
    weighted.mean(x, w = w, na.rm = TRUE)
  )
})

# --- median() ---------------------------------------------------------------

test_that("median() of an odd-length integer vecvec matches base median()", {
  x <- c(3L, 1L, 4L, 1L, 5L)
  expect_equal(median(vecvec(x)), median(x))
})

test_that("median() of an even-length double vecvec matches base median()", {
  x <- c(1.0, 2.0, 3.0, 4.0)
  expect_equal(median(vecvec(x)), median(x))
})

test_that("median() of a multi-segment vecvec matches base median()", {
  x <- 1:5
  y <- 6:10
  expect_equal(median(vecvec(x, y)), median(c(x, y)))
})

test_that("median() with NA returns NA by default", {
  x <- c(1, NA, 3)
  expect_identical(median(vecvec(x)), NA_real_)
})

test_that("median() respects na.rm = TRUE", {
  x <- c(1, NA, 3, 5)
  expect_equal(median(vecvec(x), na.rm = TRUE), median(x, na.rm = TRUE))
})

# --- var() ------------------------------------------------------------------

test_that("var() of a single-segment double vecvec matches base var()", {
  x <- c(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)
  expect_equal(var(vecvec(x)), var(x))
})

test_that("var() of an integer vecvec matches base var()", {
  x <- 1:10
  expect_equal(var(vecvec(x)), var(x))
})

test_that("var() of a multi-segment vecvec matches base var()", {
  x <- c(1.0, 2.0, 3.0)
  y <- c(4.0, 5.0, 6.0)
  expect_equal(var(vecvec(x, y)), var(c(x, y)))
})

test_that("var() of a length-1 vecvec returns NA", {
  expect_identical(var(vecvec(42)), NA_real_)
})

test_that("var() with NA returns NA by default", {
  x <- c(1, NA, 3)
  expect_identical(var(vecvec(x)), NA_real_)
})

test_that("var() respects na.rm = TRUE", {
  x <- c(1, NA, 3, 4)
  expect_equal(var(vecvec(x), na.rm = TRUE), var(x, na.rm = TRUE))
})

# --- quantile() -------------------------------------------------------------

test_that("quantile() default probs match base quantile()", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  expect_equal(quantile(vecvec(x)), quantile(x))
})

test_that("quantile() with custom probs matches base quantile()", {
  x <- 1:100
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  expect_equal(quantile(vecvec(x), probs = probs), quantile(x, probs = probs))
})

test_that("quantile() of a multi-segment vecvec matches base quantile()", {
  x <- c(1.0, 3.0, 5.0)
  y <- c(2.0, 4.0, 6.0)
  expect_equal(quantile(vecvec(x, y)), quantile(c(x, y)))
})

test_that("quantile() respects names = FALSE", {
  x <- 1:10
  expect_equal(
    quantile(vecvec(x), names = FALSE),
    quantile(x, names = FALSE)
  )
})

test_that("quantile() respects na.rm = TRUE", {
  x <- c(1, NA, 3, 4, 5)
  expect_equal(
    quantile(vecvec(x), na.rm = TRUE),
    quantile(x, na.rm = TRUE)
  )
})

test_that("quantile() with a single prob returns a named scalar", {
  x <- 1:10
  result <- quantile(vecvec(x), probs = 0.5)
  expect_equal(result, quantile(x, probs = 0.5))
})

# --- cov() ------------------------------------------------------------------

test_that("cov() with vecvec x and plain vector y matches base cov()", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(2.0, 4.0, 5.0, 4.0, 5.0)
  expect_equal(cov(vecvec(x), y = y), cov(x, y = y))
})

test_that("cov() with both x and y as vecvec matches base cov()", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(5.0, 4.0, 3.0, 2.0, 1.0)
  expect_equal(cov(vecvec(x), y = vecvec(y)), cov(x, y = y))
})

test_that("cov() of a multi-segment vecvec x matches base cov()", {
  x <- c(1.0, 2.0, 3.0)
  y_seg <- c(4.0, 5.0, 6.0)
  y <- c(2.0, 4.0, 6.0, 5.0, 3.0, 1.0)
  expect_equal(cov(vecvec(x, y_seg), y = y), cov(c(x, y_seg), y = y))
})

test_that("cov() respects use = 'complete.obs' with NAs", {
  x <- c(1, NA, 3, 4, 5)
  y <- c(2, 4,  5, 4, 5)
  expect_equal(
    cov(vecvec(x), y = y, use = "complete.obs"),
    cov(x, y = y, use = "complete.obs")
  )
})

# --- cor() ------------------------------------------------------------------
test_that("cor() with vecvec x and plain vector y matches base cor()", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(2.0, 4.0, 5.0, 4.0, 5.0)
  expect_equal(cor(vecvec(x), y = y), cor(x, y = y))
})

test_that("cor() with both x and y as vecvec matches base cor()", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(5.0, 4.0, 3.0, 2.0, 1.0)
  expect_equal(cor(vecvec(x), y = vecvec(y)), cor(x, y = y))
})

test_that("cor() of perfectly correlated vectors returns 1", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  expect_equal(cor(vecvec(x), y = x), 1)
})

test_that("cor() of perfectly anti-correlated vectors returns -1", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- rev(x)
  expect_equal(cor(vecvec(x), y = y), -1)
})

test_that("cor() of a multi-segment vecvec x matches base cor()", {
  x <- c(1.0, 2.0, 3.0)
  y_seg <- c(4.0, 5.0, 6.0)
  y <- c(2.0, 4.0, 6.0, 5.0, 3.0, 1.0)
  expect_equal(cor(vecvec(x, y_seg), y = y), cor(c(x, y_seg), y = y))
})

test_that("cor() respects use = 'complete.obs' with NAs", {
  x <- c(1, NA, 3, 4, 5)
  y <- c(2,  4, 5, 4, 5)
  expect_equal(
    cor(vecvec(x), y = y, use = "complete.obs"),
    cor(x, y = y, use = "complete.obs")
  )
})

test_that("cor() respects method = 'spearman'", {
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  y <- c(5.0, 6.0, 7.0, 8.0, 7.0)
  expect_equal(
    cor(vecvec(x), y = y, method = "spearman"),
    cor(x, y = y, method = "spearman")
  )
})