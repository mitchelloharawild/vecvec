test_that("format() on empty vecvec returns character(0)", {
  expect_identical(format(vecvec()), character(0))
})

test_that("format() on non-empty vecvec returns formatted strings", {
  expect_identical(format(vecvec(1:3, letters[1:3])), c("1", "2", "3", "a", "b", "c"))
})

test_that("print() on empty vecvec produces no output beyond the header", {
  expect_snapshot(print(vecvec()))
})

test_that("print() on non-empty vecvec produces expected output", {
  expect_snapshot(print(vecvec(1:3, letters[1:3])))
})

test_that("print() on vecvec vector respects max and shows footer", {
  expect_snapshot(print(vecvec(1:10), max = 4L))
})

test_that("print() on 2D vecvec matrix respects max (1 complete row shown)", {
  x <- array(vecvec(1:9), dim = c(3L, 3L))
  expect_snapshot(print(x, max = 4L))
})

test_that("print() on 3D vecvec array respects max (identical structure to base array)", {
  # This is the canonical example from the task: results should match
  # print(array(1:18, dim = c(3, 3, 2)), max = 6) in structure and values.
  x <- array(vecvec(1:18), dim = c(3L, 3L, 2L))
  expect_snapshot(print(x, max = 6L))
})

test_that("print() on 3D vecvec array without truncation shows all slices", {
  x <- array(vecvec(1:18), dim = c(3L, 3L, 2L))
  expect_snapshot(print(x, max = 999L))
})
