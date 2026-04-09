test_that("vctrs::vec_c", {
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

test_that("vctrs::vec_init", {
  vec <- vec_init(class_vecvec(), 5)
  expect_all_true(is.na(vec))
  expect_equal(
    as.numeric(vec),
    rep(NA_real_, 5)
  )
})


test_that("vctrs::vec_rep", {
  rand <- rnorm(5, sd = 5)
  expect_equal(
    as.numeric(vec_rep(vecvec(1:10, rand), 5)),
    rep(c(1:10, rand), 5)
  )
})
