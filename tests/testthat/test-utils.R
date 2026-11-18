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
