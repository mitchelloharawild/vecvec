test_that("Unary Ops", {
  expect_equal(
    as.numeric(-vecvec(1:10)),
    -1:-10
  )
  expect_equal(
    as.numeric(+vecvec(1:10)),
    1:10
  )
})

test_that("Binary Ops with single segments", {
  expect_equal(
    as.numeric(vecvec(1:10) + 1:10),
    seq(2, 20, by = 2)
  )
  expect_equal(
    as.numeric(vecvec(1:10) + vecvec(1:10)),
    seq(2, 20, by = 2)
  )
})

test_that("Binary Ops with scalars", {
  expect_equal(
    as.numeric(vecvec(1:10) + 5),
    6:15
  )

  rand <- rbeta(5, 3, 7)
  expect_equal(
    vecvec(-5:9, rand) >= 0,
    c(rep(FALSE, 5), rep(TRUE, 15))
  )
})

test_that("Binary Ops with overlapping segments", {
  vecvec1 <- vecvec(1:10, 11:20)
  vecvec2 <- vecvec(5:14, 15:24)
  expect_equal(
    as.numeric(vecvec1 + vecvec2),
    1:20 + 5:24
  )
})

test_that("Binary Ops with non-overlapping segments", {
  vecvec1 <- vecvec(1:5, 6:20)
  vecvec2 <- vecvec(21:30, 31:40)
  expect_equal(
    as.numeric(vecvec1 + vecvec2),
    1:20 + 21:40
  )
})
