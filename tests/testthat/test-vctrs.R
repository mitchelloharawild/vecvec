# vec_c -----------------------------------------------------------------

test_that("vctrs::vec_c", {
  expect_equal(
    vecvec(letters, 1:10),
    vec_c(vecvec(letters), vecvec(1:10))
  )
  expect_equal(
    vecvec(letters, 1:10),
    vec_c(vecvec(letters), 1:10)
  )
  expect_equal(
    vecvec(letters, 1:10),
    vec_c(letters, vecvec(1:10))
  )
})

# vec_init --------------------------------------------------------------

test_that("vctrs::vec_init", {
  vec <- vec_init(class_vecvec(), 5)
  expect_all_true(is.na(vec))
  expect_equal(
    as.numeric(vec),
    rep(NA_real_, 5)
  )
})

# vec_rep --------------------------------------------------------------

test_that("vctrs::vec_rep", {
  rand <- rnorm(5, sd = 5)
  expect_equal(
    as.numeric(vec_rep(vecvec(1:10, rand), 5)),
    rep(c(1:10, rand), 5)
  )
})

# vec_size ----------------------------------------------------------------

test_that("vctrs::vec_size", {
  expect_equal(vec_size(vecvec(1:5)), 5L)
  expect_equal(vec_size(vecvec(letters, 1:10)), 36L)
  expect_equal(vec_size(vecvec()), 0L)
  expect_equal(vec_size(vecvec(integer(0))), 0L)
})

# vec_slice ---------------------------------------------------------------

test_that("vctrs::vec_slice basic", {
  vv <- vecvec(1:5, 6:10)
  expect_equal(as.integer(vec_slice(vv, 1:5)), 1:5)
  expect_equal(as.integer(vec_slice(vv, 6:10)), 6:10)
  expect_equal(as.integer(vec_slice(vv, c(1L, 10L))), c(1L, 10L))
})

test_that("vctrs::vec_slice with mixed types", {
  vv <- vecvec(1:5, letters)
  expect_equal(as.integer(vec_slice(vv, 1:5)), 1:5)
  expect_equal(as.character(vec_slice(vv, 6:10)), letters[1:5])
})

test_that("vctrs::vec_slice out-of-order indices", {
  vv <- vecvec(1:5, 6:10)
  idx <- c(10L, 3L, 7L, 1L)
  expect_equal(as.integer(vec_slice(vv, idx)), c(10L, 3L, 7L, 1L))
})

test_that("vctrs::vec_slice zero-length result", {
  vv <- vecvec(1:5)
  expect_equal(vec_size(vec_slice(vv, integer(0))), 0L)
})

test_that("vctrs::vec_slice preserves vecvec class", {
  vv <- vecvec(1:5, letters)
  expect_true(is_vecvec(vec_slice(vv, 1:3)))
})

# proxy / restore roundtrip -----------------------------------------------

test_that("vctrs proxy/restore roundtrip is lossless", {
  vv <- vecvec(1:5, letters)
  expect_equal(vec_restore(vec_proxy(vv), vv), vv)
})

test_that("vctrs proxy/restore roundtrip: numeric only", {
  vv <- vecvec(rnorm(10))
  expect_equal(vec_restore(vec_proxy(vv), vv), vv)
})

test_that("vctrs proxy/restore roundtrip: zero-length", {
  vv <- class_vecvec()
  expect_equal(vec_restore(vec_proxy(vv), vv), vv)
})

# vec_ptype2 --------------------------------------------------------------

test_that("vctrs::vec_ptype2 vecvec x vecvec", {
  a <- vecvec(1:3)
  b <- vecvec(letters)
  pt <- vec_ptype2(a, b)
  expect_true(is_vecvec(pt))
  expect_equal(vec_size(pt), 0L)
})

test_that("vctrs::vec_ptype2 is commutative", {
  a <- vecvec(1:3)
  b <- vecvec(letters)
  expect_equal(vec_ptype2(a, b), vec_ptype2(b, a))
})

# vec_cast ----------------------------------------------------------------

# vecvec -> vecvec
test_that("vctrs::vec_cast vecvec to vecvec is identity", {
  vv <- vecvec(1:5, letters)
  expect_equal(vec_cast(vv, class_vecvec()), vv)
})

test_that("vctrs::vec_cast vecvec<integer> to vecvec<integer>", {
  vv <- vecvec(1L, 5L, 10L)
  result <- vec_cast(vv, vecvec(0L))
  expect_true(is_vecvec(result))
  expect_equal(as.integer(result), c(1L, 5L, 10L))
})

test_that("vctrs::vec_cast vecvec<character> to vecvec<character>", {
  vv <- vecvec(letters[1:3])
  result <- vec_cast(vv, vecvec(""))
  expect_true(is_vecvec(result))
  expect_equal(as.character(result), letters[1:3])
})

# plain type -> vecvec
test_that("vctrs::vec_cast plain vector to vecvec", {
  result <- vec_cast(1:5, class_vecvec())
  expect_true(is_vecvec(result))
  expect_equal(as.integer(result), 1:5)
})

test_that("vctrs::vec_cast integer to vecvec<integer>", {
  result <- vec_cast(10L, vecvec(1L))
  expect_true(is_vecvec(result))
  expect_equal(as.integer(result), 10L)
})

test_that("vctrs::vec_cast double to vecvec<double>", {
  result <- vec_cast(3.14, vecvec(1.0))
  expect_true(is_vecvec(result))
  expect_equal(as.double(result), 3.14)
})

test_that("vctrs::vec_cast character to vecvec<character>", {
  result <- vec_cast("hello", vecvec("world"))
  expect_true(is_vecvec(result))
  expect_equal(as.character(result), "hello")
})

test_that("vctrs::vec_cast logical to vecvec<logical>", {
  result <- vec_cast(c(TRUE, FALSE, NA), vecvec(TRUE))
  expect_true(is_vecvec(result))
  expect_equal(as.logical(result), c(TRUE, FALSE, NA))
})

# vecvec -> plain type
test_that("vctrs::vec_cast vecvec to integer", {
  vv <- vecvec(1L, 2L, 3L)
  result <- vec_cast(vv, integer())
  expect_equal(result, 1:3)
})

test_that("vctrs::vec_cast vecvec to double", {
  vv <- vecvec(c(1.5, 2.5, 3.5))
  result <- vec_cast(vv, double())
  expect_equal(result, c(1.5, 2.5, 3.5))
})

test_that("vctrs::vec_cast vecvec to character", {
  vv <- vecvec(c("a", "b", "c"))
  result <- vec_cast(vv, character())
  expect_equal(result, c("a", "b", "c"))
})

test_that("vctrs::vec_cast vecvec to logical", {
  vv <- vecvec(c(TRUE, FALSE))
  result <- vec_cast(vv, logical())
  expect_equal(result, c(TRUE, FALSE))
})

# zero-length edge cases
test_that("vctrs::vec_cast empty vecvec to vecvec is zero-length", {
  result <- vec_cast(class_vecvec(), class_vecvec())
  expect_true(is_vecvec(result))
  expect_equal(vec_size(result), 0L)
})

test_that("vctrs::vec_cast empty vector to vecvec is zero-length", {
  result <- vec_cast(integer(0), class_vecvec())
  expect_true(is_vecvec(result))
  expect_equal(vec_size(result), 0L)
})

# ptype labels ------------------------------------------------------------

test_that("vctrs::vec_ptype_abbr for mixed vecvec", {
  vv <- vecvec(1:3, letters)
  expect_equal(vec_ptype_abbr(vv), "vecvec")
})

test_that("vctrs::vec_ptype_full for mixed vecvec", {
  vv <- vecvec(1:3, letters)
  expect_equal(vec_ptype_full(vv), "vecvec")
})

test_that("vctrs::vec_ptype_abbr for single-type vecvec", {
  vv <- vecvec(c(1.5, 2.5))
  expect_equal(vec_ptype_abbr(vv), paste0(vec_ptype_abbr(numeric()), "*"))
})

test_that("vctrs::vec_ptype_full for single-type vecvec", {
  vv <- vecvec(c(1.5, 2.5))
  expect_equal(vec_ptype_full(vv), paste0(vec_ptype_full(numeric()), "*"))
})

# vec_detect_complete -----------------------------------------------------

test_that("vctrs::vec_detect_complete: no NAs", {
  vv <- vecvec(1:5)
  expect_true(all(vec_detect_complete(vv)))
})

test_that("vctrs::vec_detect_complete: with NAs", {
  vv <- vecvec(c(1, NA, 3))
  result <- vec_detect_complete(vv)
  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("vctrs::vec_detect_complete: all NAs", {
  vv <- vec_init(class_vecvec(), 3L)
  expect_true(all(!vec_detect_complete(vv)))
})

# vec_unique --------------------------------------------------------------

test_that("vctrs::vec_unique removes duplicates", {
  vv <- vec_c(vecvec(1:3), vecvec(1:3))
  u <- vec_unique(vv)
  expect_equal(vec_size(u), 3L)
  expect_equal(as.integer(u), 1:3)
})

test_that("vctrs::vec_unique on already-unique vecvec", {
  vv <- vecvec(1:5)
  expect_equal(vec_size(vec_unique(vv)), 5L)
})

# vec_order / vec_sort_indices --------------------------------------------

test_that("vctrs::vec_order ascending", {
  vv <- vecvec(c(3L, 1L, 2L))
  expect_equal(vec_order(vv), c(2L, 3L, 1L))
})

test_that("vctrs::vec_order descending", {
  vv <- vecvec(c(3L, 1L, 2L))
  expect_equal(vec_order(vv, direction = "desc"), c(1L, 3L, 2L))
})

test_that("vctrs::vec_sort ascending", {
  vv <- vecvec(c(3L, 1L, 4L, 1L, 5L))
  expect_equal(as.integer(vec_sort(vv)), c(1L, 1L, 3L, 4L, 5L))
})
