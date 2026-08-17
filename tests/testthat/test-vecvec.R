test_that("unvecvec", {
  expect_equal(
    unvecvec(x <- vecvec(1:5, pi, exp(1))),
    y <- c(1:5, pi, exp(1))
  )

  expect_equal(
    unvecvec(x[c(3, 7, 4, 5, 6, 2, 1)]),
    y[c(3, 7, 4, 5, 6, 2, 1)]
  )
})

test_that("unvecvec zero-length", {
  expect_equal(
    unvecvec(vecvec()),
    NULL
  )
  expect_equal(
    unvecvec(vecvec(numeric(0))),
    numeric(0)
  )
})

test_that("unvecvec missing values", {
  expect_equal(
    unvecvec(vecvec(c(1, NA, 3), c(NA, 5))),
    c(1, NA, 3, NA, 5)
  )
})

test_that("unvecvec row-scatters matrix and data.frame results", {
  # `[<-` on a matrix/data.frame is column selection, not row selection, so
  # `unvecvec()` must use vec_slice()/vec_assign() (row-respecting) to scatter
  # values into place instead of single-bracket indexing.
  m <- matrix(1:8, nrow = 4, ncol = 2, byrow = TRUE)
  vv <- class_vecvec(x = list(m), i = c(1L, 3L, 2L, 4L, 3L))
  expect_equal(
    unname(unvecvec(vv)),
    unname(m[c(1, 3, 2, 4, 3), , drop = FALSE])
  )

  df <- data.frame(a = 1:4, b = letters[1:4])
  vvd <- class_vecvec(x = list(df), i = c(1L, NA, 3L, 2L))
  expect_equal(
    unvecvec(vvd),
    data.frame(a = c(1L, NA, 3L, 2L), b = c("a", NA, "c", "b"))
  )
})


test_that("Replicating vectors", {
  rand <- rnorm(5, sd = 5)
  expect_equal(
    as.numeric(rep(vecvec(1:10, rand), 5)),
    rep(c(1:10, rand), 5)
  )
})
