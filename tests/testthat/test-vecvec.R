
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
