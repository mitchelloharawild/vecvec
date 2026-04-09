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
