test_that("vecvec_mapply applies .f element-wise (correctness), batched by shared slots", {
  x <- vecvec(c(1, 2, 3))
  y <- vecvec(c(10, 20, 30))

  r <- vecvec_mapply(list(x, y), `+`)
  expect_equal(as.numeric(r), c(11, 22, 33))

  r_novv <- vecvec_mapply(list(x, c(10, 20, 30)), `+`)
  expect_equal(as.numeric(r_novv), c(11, 22, 33))
})

test_that("vecvec_mapply calls .f once per group of shared slots, not once per element", {
  # A structured input with 3 underlying slots should only need 3 calls to
  # `.f`, regardless of how many elements it represents - this is the whole
  # point of batching by vecvec_align() groups rather than mapply()-ing
  # element by element.
  x <- vecvec(1:10, as.double(11:20), 21:30)

  calls <- 0L
  r <- vecvec_mapply(list(x, x), function(a, b) {
    calls <<- calls + 1L
    a + b
  })

  expect_equal(calls, 3L)
  expect_equal(as.numeric(r), 2 * c(1:10, 11:20, 21:30))
})

test_that("vecvec_mapply propagates NA from a missing index instead of dropping later positions", {
  # A missing (NA) index anywhere in an input must not corrupt vecvec_align()
  # such that every later position silently vanishes from the result.
  x <- vecvec(c(1, 2, 3, 10))[c(1, 2, 3, NA, 4)]
  y <- vecvec(10 * (1:5))

  r <- vecvec_mapply(list(x, y), `+`)
  expect_length(r, 5L)
  expect_equal(as.numeric(r), c(11, 22, 33, NA, 60))
})

test_that("vecvec_mapply errors when .f doesn't return one result per group position", {
  x <- vecvec(1:10, as.double(11:20))

  expect_error(
    vecvec_mapply(list(x), sum),
    class = "rlang_error"
  )
})

test_that("vecvec_mapply handles empty and fully-missing inputs", {
  e <- vecvec(numeric(0))
  expect_length(vecvec_mapply(list(e, e), `+`), 0L)

  na_only <- vecvec(c(1, 2))[c(NA, NA)]
  r <- vecvec_mapply(list(na_only, na_only), `+`)
  expect_equal(as.numeric(r), c(NA_real_, NA_real_))
})
