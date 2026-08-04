test_that("is_altrep() detects ALTREP vectors", {
  expect_true(is_altrep(seq_len(10)))
  expect_true(is_altrep(1:10))
  expect_true(is_altrep(seq(1, 10)))
  expect_true(is_altrep(as.character(seq_len(5))))
})

test_that("is_altrep() detects materialised (non-ALTREP) vectors", {
  expect_false(is_altrep(c(1, 2, 3)))
  expect_false(is_altrep(c(1L, 2L, 3L)))
  expect_false(is_altrep(1:10 + 0.5))
  expect_false(is_altrep(rev(seq_len(10))))
  expect_false(is_altrep(structure(seq_len(5), class = "foo")))
})

test_that("vecvec_flatten_adj() merges adjacent same-ptype materialised slots", {
  # NB: use c(1L, 2L, 3L) rather than 1:3 -- integer range literals like
  # 1:3 are themselves ALTREP compact sequences and would correctly be
  # excluded from merging (see test below).
  x <- vecvec_flatten_adj(list(c(1L, 2L, 3L), c(4L, 5L, 6L), "a"))
  expect_equal(length(x), 2L)
  expect_equal(x[[1]], 1:6)
  expect_equal(x[[2]], "a")
})

test_that("vecvec_flatten_adj() does not merge adjacent ALTREP slots", {
  x <- vecvec_flatten_adj(list(seq_len(3), seq_len(3)))
  expect_equal(length(x), 2L)
})

test_that("vecvec() construction merges adjacent non-ALTREP slots of the same type", {
  x <- vecvec(1.5, 2.5, 3.5)
  expect_equal(length(x@x), 1L)
  expect_equal(unvecvec(x), c(1.5, 2.5, 3.5))
})
