test_that("unvecvec", {
  expect_equal(
    unvecvec(x <- vecvec(1:5, pi, exp(1))),
    y <- c(1:5, pi, exp(1))
  )

  expect_equal(
    unvecvec(x[c(3,7,4,5,6,2,1)]),
    y[c(3,7,4,5,6,2,1)]
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


test_that("Replicating vectors", {
  rand <- rnorm(5, sd = 5)
  expect_equal(
    as.numeric(rep(vecvec(1:10, rand), 5)),
    rep(c(1:10, rand), 5)
  )
})