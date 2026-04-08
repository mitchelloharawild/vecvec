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