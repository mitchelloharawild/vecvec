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
