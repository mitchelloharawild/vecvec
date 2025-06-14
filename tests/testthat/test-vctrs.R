test_that("vec_c", {
  expect_equal(
    new_vecvec(letters, 1:10),
    vec_c(new_vecvec(letters), new_vecvec(1:10))
  )
  expect_equal(
    new_vecvec(letters, 1:10),
    vec_c(new_vecvec(letters), 1:10)
  )
  expect_equal(
    new_vecvec(letters, 1:10),
    vec_c(letters, new_vecvec(1:10))
  )
})


test_that("vec_arith", {
  expect_equal(
    as.numeric(new_vecvec(1:10) + 1:10),
    seq(2,20, by = 2)
  )
  expect_equal(
    as.numeric(-new_vecvec(1:10)),
    -1:-10
  )
  rand <- rbeta(5, 3, 7)
  expect_equal(
    new_vecvec(-5:9, rand) >= 0,
    c(rep(FALSE, 5), rep(TRUE, 15))
  )
  expect_equal(
    log(new_vecvec(1:10, rand)),
    log(c(1:10, rand))
  )
})

test_that("vec_sort", {
  rand <- rnorm(5, sd = 5)
  expect_equal(
    as.numeric(sort(new_vecvec(1:10, rand))),
    sort(c(1:10, rand))
  )
  expect_equal(
    as.numeric(sort(sample(new_vecvec(1:10, rand)))),
    sort(c(1:10, rand))
  )
})


test_that("unvecvec", {
  expect_equal(
    unvecvec(x <- new_vecvec(1:5, pi, exp(1))),
    y <- c(1:5, pi, exp(1))
  )

  expect_equal(
    unvecvec(x[c(3,7,4,5,6,2,1)]),
    y[c(3,7,4,5,6,2,1)]
  )
})
