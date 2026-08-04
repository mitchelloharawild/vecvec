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

# duplicated indices in `to` (#18-adjacent: compressed vecvecs as targets)
test_that("vctrs::vec_cast plain vector to vecvec with duplicated indices (single slot)", {
  to <- rep(vecvec(1.5), 3)
  expect_equal(S7_data(to), c(1L, 1L, 1L))

  result <- vec_cast(c(1, 2, 3), to)
  expect_true(is_vecvec(result))
  expect_equal(vec_size(result), 3L)
  expect_equal(unvecvec(result), c(1, 2, 3))
})

test_that("vctrs::vec_cast plain vector to vecvec with duplicated indices casts to slot type", {
  # Integer input cast against a double-typed slot should produce doubles
  to <- rep(vecvec(1.5), 3)
  result <- vec_cast(c(1L, 2L, 3L), to)
  expect_true(is_vecvec(result))
  expect_true(is.double(result@x[[1L]]))
  expect_equal(unvecvec(result), c(1, 2, 3))
})

test_that("vctrs::vec_cast to vecvec with duplicated indices still works when values coincide", {
  to <- rep(vecvec(1.5), 3)
  result <- vec_cast(c(5, 5, 5), to)
  expect_true(is_vecvec(result))
  expect_equal(unvecvec(result), c(5, 5, 5))
})

test_that("vctrs::vec_cast to vecvec with duplicated indices across multiple mixed-type slots", {
  to <- c(rep(vecvec(1.5), 2), rep(vecvec(2L), 2))
  expect_equal(S7_data(to), c(1L, 1L, 2L, 2L))

  result <- vec_cast(c(10L, 20L, 30L, 40L), to)
  expect_true(is_vecvec(result))
  expect_equal(vec_size(result), 4L)
  expect_equal(unvecvec(result), c(10, 20, 30, 40))
  # First slot (double ptype) keeps its cast values as doubles
  expect_true(is.double(result@x[[1L]]))
  # Second slot (integer ptype) keeps its cast values as integers
  expect_true(is.integer(result@x[[2L]]))
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

# vec_proxy_equal -----------------------------------------------------------
# `vec_proxy_equal()` used to build one R object per stored value via
# `as.list(x)`, which is O(unique stored values) dispatch-and-allocate calls
# and dominates vec_match()/vec_in()/vec_group_id()/vec_count() (and hence
# dplyr joins/group_by()) on a vecvec column. It is rewritten to compute a
# proxy per group of same-ptype slots and scatter it into `length(x)` rows
# via vectorized slicing instead of looping over individual elements.

test_that("vctrs::vec_proxy_equal detects duplicates within a single type", {
  vv <- vecvec(c(3, 1, 2, 1, 3))
  # vec_group_id()'s group ids only carry the same information as identity
  # up to relabelling, so compare via match() rather than exact values.
  expect_equal(match(vec_group_id(vv), unique(vec_group_id(vv))), c(1L, 2L, 3L, 2L, 1L))
  # vec_duplicate_detect() flags every element that shares its value with
  # another element, including the first occurrence (unlike base duplicated()).
  expect_equal(as.logical(vec_duplicate_detect(vv)), c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(vec_match(vv, vv), c(1L, 2L, 3L, 2L, 1L))
})

test_that("vctrs::vec_proxy_equal never equates values from different ptypes", {
  # `5L` and `"5"` are stored in different slots and must never be treated
  # as duplicates, matching how `duplicated()` only ever compares within a
  # common-ptype group of slots.
  vv <- vecvec(5L, "5")
  expect_false(vec_group_id(vv)[[1]] == vec_group_id(vv)[[2]])
  expect_false(as.logical(vec_duplicate_detect(vv))[[1]])
})

test_that("vctrs::vec_proxy_equal unifies same-ptype values across non-adjacent slots", {
  # Slots 1 and 3 share a ptype (integer) but are separated by a character
  # slot, so they are not merged by vecvec_flatten_adj(); the value 3 stored
  # in slot 1 and the value 3 stored in slot 3 must still compare equal.
  vv <- vecvec(1:3, letters[1:3], c(3L, 5L, 9L))
  g <- vec_group_id(vv)
  expect_equal(g[[3]], g[[7]])
  expect_equal(length(unique(g)), 8L)
  expect_true(as.logical(vec_duplicate_detect(vv))[[7]])
})

test_that("vctrs::vec_proxy_equal keeps unassigned (NA-index) elements mutually equal", {
  vv <- vec_init(class_vecvec(), 4L)
  expect_true(all(vec_group_id(vv) == vec_group_id(vv)[[1]]))
  expect_true(all(as.logical(vec_duplicate_detect(vv))))
})

test_that("vctrs::vec_proxy_equal distinguishes unassigned index from a stored NA value", {
  vv <- vecvec(c(1, NA, 3))
  vv <- vec_c(vv, vec_init(class_vecvec(), 1L))
  # element 2 (stored NA) and element 4 (unassigned index) must not be
  # grouped together, even though both are considered `is.na()`.
  expect_false(vec_group_id(vv)[[2]] == vec_group_id(vv)[[4]])
})

test_that("vctrs::vec_proxy_equal handles rcrd-typed slots without destructuring", {
  rc <- function(a, b) {
    vctrs::new_rcrd(vctrs::vec_recycle_common(a = a, b = b), class = "myrcrd")
  }
  vv <- vecvec(rc(c(1, 1, 2), 1))
  g <- vec_group_id(vv)
  expect_equal(g[[1]], g[[2]])
  expect_false(g[[1]] == g[[3]])
  expect_equal(as.logical(vec_duplicate_detect(vv)), c(TRUE, TRUE, FALSE))

  vv2 <- vecvec(rc(c(1, 1, 2), 1), rc(c(2, 3), 1))
  g2 <- vec_group_id(vv2)
  expect_equal(g2[[1]], g2[[2]])
  expect_equal(g2[[3]], g2[[4]])
  expect_equal(length(unique(g2)), 3L)
})

test_that("vctrs::vec_proxy_equal matches ground truth for a randomised mixed-type vecvec", {
  set.seed(42)
  vv <- vecvec(
    sample(letters[1:4], 20, replace = TRUE),
    sample(1:3, 15, replace = TRUE)
  )
  g <- as.integer(vec_group_id(vv))
  grp_sizes <- table(g)
  expected_dup <- unname(as.vector(grp_sizes[as.character(g)] > 1))
  expect_equal(as.logical(vec_duplicate_detect(vv)), expected_dup)
  expect_equal(vec_match(vv, vv), match(g, g))
  expect_equal(vec_size(vec_unique(vv)), length(unique(g)))
})

test_that("vctrs::vec_proxy_equal is fast for many unique stored values", {
  skip_on_cran()
  big <- vecvec(vctrs::new_rcrd(list(a = 1:20000), class = "myrcrd"))
  elapsed <- system.time(vctrs::vec_proxy_equal(big))[["elapsed"]]
  expect_lt(elapsed, 0.1)
})

test_that("vctrs::vec_proxy_equal stays fast for a highly compressed vecvec", {
  skip_on_cran()
  comp <- rep(vecvec(1.5), 20000)
  elapsed <- system.time(vctrs::vec_proxy_equal(comp))[["elapsed"]]
  expect_lt(elapsed, 0.1)
})
