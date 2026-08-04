test_that("Unary Math with single types", {
  rand <- rbeta(5, 3, 7)
  expect_equal(
    as.numeric(log(vecvec(rand))),
    log(rand)
  )
})

test_that("Cumulative Math generics abort instead of returning wrong values", {
  v <- vecvec(1:3, c(10, 20))

  expect_error(cumsum(v), "cumsum")
  expect_error(cumprod(v), "cumprod")
  expect_error(cummax(v), "cummax")
  expect_error(cummin(v), "cummin")
})

test_that("Non-cumulative Math generics still work on mixed-type vecvecs", {
  v <- vecvec(-1:1, c(-5, 5))

  expect_equal(
    as.numeric(abs(v)),
    abs(c(-1:1, -5, 5))
  )
  expect_equal(
    as.numeric(sqrt(vecvec(c(1, 4, 9)))),
    sqrt(c(1, 4, 9))
  )
})
