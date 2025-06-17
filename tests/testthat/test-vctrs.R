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
    as.numeric(log(vecvec(1:10, rand))),
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

test_that("vec_rep", {
  rand <- rnorm(5, sd = 5)
  expect_equal(
    as.numeric(rep(vecvec(1:10, rand), 5)),
    rep(c(1:10, rand), 5)
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

test_that("vec_restore with missing", {
  expect_equal(
    sum(is.na(sample(c(vec_init(new_vecvec(), 5), rnorm(10))))),
    5
  )
})

test_that("duplicate detection", {
  x <- vec_c(1L, vecvec(1:10))
  expect_true(
    anyDuplicated(x)
  )
  expect_identical(
    duplicated(x),
    c(FALSE, TRUE, rep(FALSE, 9))
  )
  expect_identical(
    unique(x),
    vecvec(1:10)
  )
})
