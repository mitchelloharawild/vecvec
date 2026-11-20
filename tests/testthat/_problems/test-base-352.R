# Extracted from test-base.R:352

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vecvec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
c_copy <- function(x, i) {
  data <- unclass(x)
  attributes(data) <- NULL
  out <- data[i]
  attributes(out) <- attributes(x)[setdiff(names(attributes(x)), c("dim", "dimnames", "names"))]
  out
}
model_data <- function(...) {
  data.frame(y = c(1, 3, 5, 7, 9, 12), z = c(2, 1, 4, 3, 6, 5), ...)
}

# test -------------------------------------------------------------------------
for (v in list(c(1.5, NA, 3), c(2L, NA), c(TRUE, NA), c("a", NA), c(1i, NA), as.raw(1:2),
                 as.Date("2024-01-01") + 0:1, factor(c("p", "q")))) {
    x <- vecvec(v)
    expect_identical(c(x, x), vecvec(c(v, v)))
    expect_identical(c(x, x[1]), vecvec(c(v, v[1])))
    expect_identical(rep(x, 2), vecvec(rep(v, 2)))
    expect_identical(rep(x, each = 2, length.out = 3), vecvec(rep(v, each = 2, length.out = 3)))
    expect_identical(is.na(x), is.na(v))
  }
x <- vecvec(c(a = 1, b = NA))
expect_identical(is.na(x), c(a = FALSE, b = TRUE))
