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
