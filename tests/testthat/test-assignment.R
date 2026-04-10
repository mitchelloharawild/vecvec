# `[<-` -------------------------------------------------------------------

test_that("`[<-` replaces a single element with a plain vector", {
  x <- vecvec(letters, 1:26)
  x[1] <- 99L
  expect_equal(x[[1]], 99L)
})

test_that("`[<-` replaces multiple elements with a plain vector", {
  x <- vecvec(1:10)
  x[1:3] <- c(11L, 12L, 13L)
  expect_equal(as.integer(x[1:3]), c(11L, 12L, 13L))
})

test_that("`[<-` recycles scalar value across selected indices", {
  x <- vecvec(1:5)
  x[1:3] <- 0L
  expect_equal(as.integer(x[1:3]), c(0L, 0L, 0L))
})

test_that("`[<-` preserves unmodified elements", {
  x <- vecvec(1:5)
  x[1] <- 99L
  expect_equal(as.integer(x[2:5]), 2:5)
})

test_that("`[<-` replaces elements across type boundaries", {
  x <- vecvec(letters, 1:26)
  x[25:28] <- 0L
  expect_equal(as.integer(x[25:28]), rep(0L, 4L))
})

test_that("`[<-` assigns a vecvec value", {
  x <- vecvec(letters, 1:26)
  x[1:3] <- vecvec(c(TRUE, FALSE, TRUE))
  expect_equal(as.logical(x[1:3]), c(TRUE, FALSE, TRUE))
})

test_that("`[<-` replaces with a Date value", {
  x <- vecvec(1:5)
  x[2:3] <- Sys.Date()
  expect_s3_class(x[[2]], "Date")
  expect_s3_class(x[[3]], "Date")
  expect_equal(x[[2]], x[[3]])
})

test_that("`[<-` keeps vecvec class after assignment", {
  x <- vecvec(1:5)
  x[1] <- 99L
  expect_true(is_vecvec(x))
})

test_that("`[<-` preserves length after single-element replacement", {
  x <- vecvec(letters)
  x[1] <- "z"
  expect_equal(length(x), 26L)
})

test_that("`[<-` preserves length after multi-element replacement", {
  x <- vecvec(1:10, letters)
  x[3:6] <- 0L
  expect_equal(length(x), 36L)
})

test_that("`[<-` replaces last element correctly", {
  x <- vecvec(1:5)
  x[5] <- 99L
  expect_equal(as.integer(x[5]), 99L)
  expect_equal(as.integer(x[1:4]), 1:4)
})

test_that("`[<-` replaces first element correctly", {
  x <- vecvec(1:5)
  x[1] <- 99L
  expect_equal(as.integer(x[1]), 99L)
  expect_equal(as.integer(x[2:5]), 2:5)
})

test_that("`[<-` round-trips through unvecvec after replacement", {
  x <- vecvec(1:5)
  x[3] <- 99L
  expected <- c(1L, 2L, 99L, 4L, 5L)
  expect_equal(unvecvec(x, ptype = integer()), expected)
})

# `[[<-` -------------------------------------------------------------------

test_that("`[[<-` replaces a single element", {
  x <- vecvec(letters)
  x[[1]] <- "z"
  expect_equal(x[[1]], "z")
})

test_that("`[[<-` replaces middle element", {
  x <- vecvec(1:5)
  x[[3]] <- 99L
  expect_equal(x[[3]], 99L)
})

test_that("`[[<-` preserves surrounding elements", {
  x <- vecvec(1:5)
  x[[3]] <- 99L
  expect_equal(as.integer(x[c(1, 2, 4, 5)]), c(1L, 2L, 4L, 5L))
})

test_that("`[[<-` preserves length", {
  x <- vecvec(letters)
  x[[1]] <- "z"
  expect_equal(length(x), 26L)
})

test_that("`[[<-` preserves vecvec class", {
  x <- vecvec(1:5)
  x[[1]] <- 99L
  expect_true(is_vecvec(x))
})

test_that("`[[<-` errors when multiple indices given", {
  x <- vecvec(1:5)
  expect_error(
    x[[1:2]] <- 99L,
    "attempt to replace multiple elements"
  )
})

test_that("`[[<-` works across type boundary", {
  x <- vecvec(letters, 1:26)
  x[[27]] <- 999L
  expect_equal(x[[27]], 999L)
})

# `is.na<-` ----------------------------------------------------------------

test_that("`is.na<-` sets a single element to NA by integer index", {
  x <- vecvec(1:5)
  is.na(x) <- 3L
  expect_true(is.na(x[[3]]))
})

test_that("`is.na<-` sets multiple elements to NA by integer indices", {
  x <- vecvec(1:5)
  is.na(x) <- c(1L, 3L, 5L)
  expect_equal(is.na(unvecvec(x, ptype = integer())), c(TRUE, FALSE, TRUE, FALSE, TRUE))
})

test_that("`is.na<-` sets elements to NA by logical vector", {
  x <- vecvec(1:5)
  is.na(x) <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  result <- is.na(unvecvec(x, ptype = integer()))
  expect_equal(result, c(TRUE, FALSE, TRUE, FALSE, FALSE))
})

test_that("`is.na<-` preserves non-NA elements", {
  x <- vecvec(1:5)
  is.na(x) <- 1L
  expect_equal(as.integer(x[2:5]), 2:5)
})

test_that("`is.na<-` preserves length", {
  x <- vecvec(1:5)
  is.na(x) <- 2L
  expect_equal(length(x), 5L)
})

test_that("`is.na<-` preserves vecvec class", {
  x <- vecvec(1:5)
  is.na(x) <- 1L
  expect_true(is_vecvec(x))
})

test_that("`is.na<-` errors on logical subscript of wrong length", {
  x <- vecvec(1:5)
  expect_error(
    { is.na(x) <- c(TRUE, FALSE) },
    "logical subscript has wrong length"
  )
})

test_that("`is.na<-` sets NA across type boundaries in a mixed vecvec", {
  x <- vecvec(letters, 1:5)
  is.na(x) <- c(25L, 26L, 27L)
  expect_true(is.na(x[[25]]))
  expect_true(is.na(x[[26]]))
  expect_true(is.na(x[[27]]))
})

test_that("`is.na<-` sets all elements NA with a fully-TRUE logical mask", {
  x <- vecvec(1:5)
  is.na(x) <- rep(TRUE, 5)
  expect_true(all(is.na(unvecvec(x, ptype = integer()))))
})

test_that("`is.na<-` with fully-FALSE mask leaves no NAs", {
  x <- vecvec(1:5)
  is.na(x) <- rep(FALSE, 5)
  expect_false(anyNA(unvecvec(x, ptype = integer())))
})

test_that("`is.na<-` produces NA of the correct type for character slot", {
  x <- vecvec(letters)
  is.na(x) <- 1L
  expect_true(is.na(x[[1]]))
  expect_true(is.character(x[[1]]))
})

test_that("`is.na<-` produces NA of the correct type for numeric slot", {
  x <- vecvec(rnorm(5))
  is.na(x) <- 2L
  expect_true(is.na(x[[2]]))
  expect_true(is.numeric(x[[2]]))
})
