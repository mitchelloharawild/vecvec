test_that("vecvec() stores vectors of the same type together", {
  x <- vecvec(c(1L, 2L, 3L), c(4L, 5L, 6L), "a")
  expect_length(x@x, 2L)
  expect_equal(x@x[[1]], 1:6)
  expect_equal(x@x[[2]], "a")
  expect_equal(format(x), c(as.character(1:6), "a"))
})

test_that("vecvec() stores non-adjacent vectors of the same type together", {
  x <- vecvec(1.5, "a", 2.5, "b", 3.5)
  expect_length(x@x, 2L)
  expect_equal(x@x[[1]], c(1.5, 2.5, 3.5))
  expect_equal(x@x[[2]], c("a", "b"))
  expect_equal(format(x), c("1.5", "a", "2.5", "b", "3.5"))
})

test_that("vecvec() construction combines vectors of the same type", {
  x <- vecvec(1.5, 2.5, 3.5)
  expect_equal(length(x@x), 1L)
  expect_equal(unvecvec(x), c(1.5, 2.5, 3.5))
})

test_that("vecvec() combines many vectors of the same type", {
  x <- do.call(vecvec, lapply(1:5000, function(i) c(i, i) + 0))
  expect_length(x@x, 1L)
  mixed <- vecvec(1, 2, "a", "b", 3)
  expect_equal(mixed@x, list(c(1, 2, 3), c("a", "b")))
})

test_that("vecvec_align() keeps an NA index in its own group instead of dropping everything after it", {
  x <- vecvec(c(1, 2, 3, 10), "a")[c(1, 2, 3, NA, 5)]
  y <- vecvec(as.list(10 * (1:5)))

  al <- vecvec_align(list(x, y))
  expect_equal(sort(unlist(al$groups)), 1:5)
  expect_true(any(vapply(al$groups, identical, logical(1L), 4L)))
})

test_that("vecvec_align() groups interleaved positions by slot combination", {
  n <- 50
  il <- class_vecvec(list(as.double(1:n), as.integer(1:n)), as.vector(rbind(1:n, n + 1:n)))
  al <- vecvec_align(list(il))
  expect_length(al$groups, 2L)
  expect_equal(al$groups[[1]], seq(1L, 2L * n, by = 2L))
})

test_that("vecvec_mapply() calls .f once per slot combination for interleaved input", {
  n <- 50
  il <- class_vecvec(list(as.double(1:n), as.integer(1:n)), as.vector(rbind(1:n, n + 1:n)))
  calls <- 0L
  res <- vecvec_mapply(list(il), function(v) { calls <<- calls + 1L; v + 1 }, ptype = double())
  expect_equal(calls, 2L)
  expect_equal(res, as.double(unvecvec(il, ptype = double())) + 1)

  calls <- 0L
  vecvec_mapply(list(il, il[c(NA, seq_len(2 * n - 1))]), function(a, b) { calls <<- calls + 1L; a }, ptype = double())
  expect_lte(calls, 4L)
})

test_that("Ops on interleaved vecvecs match element-wise results", {
  n <- 50
  il <- class_vecvec(list(as.double(1:n), as.integer(1:n)), as.vector(rbind(1:n, n + 1:n)))
  expect_equal(unvecvec(il + il[rev(seq_len(2 * n))], ptype = double()),
               as.double(unvecvec(il, ptype = double())) + rev(as.double(unvecvec(il, ptype = double()))))
})
