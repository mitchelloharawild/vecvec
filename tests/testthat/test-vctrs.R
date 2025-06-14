test_that("vec_c", {
  expect_equal(
    vecvec(letters, 1:10),
    vec_c(vecvec(letters), vecvec(1:10))
  )
  expect_equal(
    vecvec(letters, 1:10),
    vec_c(vecvec(letters), 1:10)
  )
  expect_equal(
    vecvec(letters, 1:10),
    vec_c(letters, vecvec(1:10))
  )
})


test_that("vec_arith", {
  expect_equal(
    as.numeric(vecvec(1:10) + 1:10),
    seq(2,20, by = 2)
  )
  expect_equal(
    as.numeric(-vecvec(1:10)),
    -1:-10
  )
  rand <- rbeta(5, 3, 7)
  expect_equal(
    vecvec(-5:9, rand) >= 0,
    c(rep(FALSE, 5), rep(TRUE, 15))
  )
  expect_equal(
    log(vecvec(1:10, rand)),
    log(c(1:10, rand))
  )
})

test_that("vec_sort", {
  rand <- rnorm(5, sd = 5)
  expect_equal(
    as.numeric(sort(vecvec(1:10, rand))),
    sort(c(1:10, rand))
  )
  expect_equal(
    as.numeric(sort(sample(vecvec(1:10, rand)))),
    sort(c(1:10, rand))
  )
})


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
