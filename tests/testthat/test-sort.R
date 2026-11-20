test_that("sort() increasing order", {
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

test_that("sort() decreasing order", {
  vv_num <- vecvec(c(1.5, 3.5, 2.5, 5.5, 4.5), c(1, 3, 2, 5, 4))
  expect_equal(
    as.numeric(sort(vv_num, decreasing = TRUE)),
    sort(as.numeric(vv_num), decreasing = TRUE)
  )
})

test_that("mixed types sort by type (in order of appearance), then value", {
  x <- vecvec("b", 1, "a", NA_character_, 0)
  expect_identical(format(sort(x)), c("a", "b", "0", "1"))
  expect_identical(order(x), c(3L, 1L, 5L, 2L, 4L))
  expect_identical(vctrs::vec_order(x), order(x))
  expect_identical(format(sort(x, decreasing = TRUE)), c("1", "0", "b", "a"))

  # Types with a common type sort by value as that type
  expect_identical(format(sort(vecvec(3L, 2.5, 1L))), c("1", "2.5", "3"))
})

test_that("vctrs compares the values of mixed-type vecvecs", {
  expect_identical(
    vctrs::vec_compare(vecvec(1, "a", 5), vecvec(2, "b", 5)),
    c(-1L, -1L, 0L)
  )
  expect_identical(vctrs::vec_compare(vecvec(3L, 2.5), vecvec(2.5, 3L)), c(1L, -1L))
})
