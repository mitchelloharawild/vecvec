# Extracted from test-arith.R:147

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vecvec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
set.seed(3)
a <- vecvec(c(1.5, NA, 3), 4:6, c(0.5, 2))
b <- vecvec(7:9, c(2.5, NA))
for (rep_a in list(1:8, sample(8, 20, TRUE), c(NA, 1:7))) {
    for (rep_b in list(1:5, sample(5, 20, TRUE), 5:1)) {
      x <- a[rep_a]
      y <- b[rep_b]
      n <- max(length(x), length(y))
      if (n %% length(x) || n %% length(y)) next
      px <- unvecvec(x)
      py <- unvecvec(y)
      expect_identical(unvecvec(x + y), px + py)
      expect_identical(unvecvec(x * y), px * py)
      expect_identical(x == y, px == py)
      expect_identical(x < y, px < py)
      expect_identical(
        unvecvec(vecvec_mapply(list(x, y), function(u, v) u - v)),
        px - py
      )
    }
  }
