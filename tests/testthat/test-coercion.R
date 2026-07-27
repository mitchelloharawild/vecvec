# as.data.frame -------------------------------------------------------------

test_that("as.data.frame() wraps a vecvec as a single column instead of casting elements", {
  vv <- vecvec(1:3, 4:6)
  df <- as.data.frame(vv)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 6L)
  expect_equal(ncol(df), 1L)
  expect_true(is_vecvec(df[[1]]))
  expect_equal(as.integer(df[[1]]), 1:6)
})

test_that("as.data.frame() derives the column name from the input expression", {
  vv <- vecvec(1:3, 4:6)
  df <- as.data.frame(vv)

  expect_equal(names(df), "vv")
})

test_that("as.data.frame() with optional = TRUE leaves the column unnamed", {
  vv <- vecvec(1:3, 4:6)
  df <- as.data.frame(vv, optional = TRUE)

  expect_null(names(df))
})

test_that("as.data.frame() respects an explicit nm argument", {
  vv <- vecvec(1:3, 4:6)
  df <- as.data.frame(vv, nm = "custom")

  expect_equal(names(df), "custom")
})

test_that("data.frame() can use a vecvec as a column without erroring", {
  vv <- vecvec(1:3, 4:6)
  df <- data.frame(x = vv)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 6L)
  expect_true(is_vecvec(df$x))
  expect_equal(as.integer(df$x), 1:6)
})

test_that("as.data.frame() works for a vecvec of mixed element types", {
  vv <- vecvec(1:3, c("a", "b", "c"))
  df <- as.data.frame(vv)

  expect_equal(nrow(df), 6L)
  expect_equal(as.character(df[[1]]), c("1", "2", "3", "a", "b", "c"))
})
