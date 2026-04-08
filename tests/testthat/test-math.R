test_that("Unary Math with single types", {
  rand <- rbeta(5, 3, 7)
  expect_equal(
    as.numeric(log(vecvec(rand))),
    log(rand)
  )
})
