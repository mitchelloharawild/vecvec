test_that("Unary Ops", {
  expect_equal(
    as.numeric(-vecvec(1:10)),
    -1:-10
  )
  expect_equal(
    as.numeric(+vecvec(1:10)),
    1:10
  )
})

test_that("Binary Ops with single segments", {
  expect_equal(
    as.numeric(vecvec(1:10) + 1:10),
    seq(2,20, by = 2)
  )
  expect_equal(
    as.numeric(vecvec(1:10) + vecvec(1:10)),
    seq(2,20, by = 2)
  )
})

test_that("Binary Ops with scalars", {
  expect_equal(
    as.numeric(vecvec(1:10) + 5),
    6:15
  )

  rand <- rbeta(5, 3, 7)
  expect_equal(
    vecvec(-5:9, rand) >= 0,
    c(rep(FALSE, 5), rep(TRUE, 15))
  )
})

test_that("Binary Ops with overlapping segments", {
  vecvec1 <- vecvec(1:10, 11:20)
  vecvec2 <- vecvec(5:14, 15:24)
  expect_equal(
    as.numeric(vecvec1 + vecvec2),
    1:20 + 5:24
  )
})

test_that("Binary Ops with non-overlapping segments", {
  vecvec1 <- vecvec(1:5, 6:20)
  vecvec2 <- vecvec(21:30, 31:40)
  expect_equal(
    as.numeric(vecvec1 + vecvec2),
    1:20 + 21:40
  )
})

test_that("Binary Ops don't expand when arguments are invariable over replicates", {
  x <- vecvec(1:10, as.double(11:20), 21:30)

  result <- rep(x, each = 10) + 100L
  result_vals <- as.numeric(result)

  expected <- rep(c(1:10, 11:20, 21:30), each = 10) + 100
  expect_equal(result_vals, expected)

  # The two integer vectors are stored together
  expect_equal(
    lengths(result@x),
    c(20L, 10L)
  )
})

test_that("Binary Ops propagate NA from a missing index in either operand", {
  # `vecvec_align()` must treat an NA slot as a run boundary rather than
  # letting it poison `cumsum()` for every position after it - and this
  # `Ops` method must in turn map those NA groups to a missing index instead
  # of trying to compute on them.
  e1 <- vecvec(c(1, 2, 3, 10))[c(1, 2, NA, 4)]
  e2 <- vecvec(c(1, 2, 3, 10))[c(1, NA, 3, 4)]

  expect_equal(as.numeric(e1 + c(100, 200, 300, 400)), c(101, 202, NA, 410))
  expect_equal(as.numeric(e1 + e2), c(2, NA, NA, 20))
  expect_equal(e1 == e2, c(TRUE, NA, NA, TRUE))
})

test_that("Binary Ops expand when arguments differ across replicated indices", {
  x <- vecvec(1:10, as.double(11:20), 21:30)

  result <- rep(x, each = 10) + 1:300
  result_vals <- as.numeric(result)

  expected <- rep(c(1:10, 11:20, 21:30), each = 10) + 1:300
  expect_equal(result_vals, expected)
  expect_equal(
    lengths(result@x),
    c(200L, 100L)
  )
})

test_that("Ops on long slots doesn't overflow the pair key", {
  n <- 1e5
  expect_no_warning(v <- as.double(unvecvec(vecvec(as.double(1:n)) + vecvec(as.double(1:n)))))
  expect_equal(v, 2 * (1:n))

  # Unbalanced slot sizes whose product passes 2^31
  a <- vecvec(as.double(1:5e4))
  b <- vecvec(as.double(1:5e4))
  expect_no_warning(r <- as.double(unvecvec(a * b)))
  expect_equal(r, as.double(1:5e4)^2)
  expect_no_warning(r <- as.double(unvecvec(a + vecvec(1))))
  expect_equal(r, as.double(1:5e4) + 1)
})

test_that("Ops computes duplicate operand pairs of mixed-type vecvecs once", {
  a <- vecvec(c(1, 2), 3L)[rep(1:3, 1e4)]
  b <- vecvec(c(10, 20), 30L)[rep(1:3, 1e4)]
  x <- a + b
  expect_equal(as.double(unvecvec(x)), rep(c(11, 22, 33), 1e4))
  expect_equal(lengths(x@x), c(2L, 1L))
})

test_that("Ops doesn't expand a size one operand of a mixed-type vecvec", {
  a <- vecvec(c(1, 2), 3L)[rep(1:3, 1e4)]
  x <- a + 1L
  expect_equal(lengths(x@x), c(2L, 1L))
})

test_that("operations of a mixed-type vecvec with a single value keep its index", {
  x <- vecvec(1:3, c(0.5, 1.5))[c(5, 1, 1, 4)]
  expect_identical(unvecvec(x + 1), unvecvec(x) + 1)
  expect_identical(unvecvec(10 - x), 10 - unvecvec(x))
  expect_identical(unvecvec(x * vecvec(2L)), unvecvec(x) * 2)
  expect_identical(x > 1, unvecvec(x) > 1)
  expect_identical(vecvec(1L, "a") == "a", c(FALSE, TRUE))
  expect_identical(vecvec(1L, "a")[c(NA, 2)] == "a", c(NA, TRUE))
})

test_that("operations between mixed-type vecvecs match the plain vectors", {
  set.seed(3)
  a <- vecvec(c(1.5, NA, 3), 4:6, c(0.5, 2))
  b <- vecvec(7:9, c(2.5, NA))
  # Sizes recycle as in vctrs (only size one)
  for (rep_a in list(sample(8, 20, TRUE), c(NA, 1:8, 8:1, 2:4), 3L)) {
    for (rep_b in list(sample(5, 20, TRUE), rep(1:5, 4), 2L)) {
      x <- a[rep_a]
      y <- b[rep_b]
      px <- unvecvec(x)
      py <- unvecvec(y)
      expect_identical(unvecvec(x + y), px + py)
      expect_identical(unvecvec(x * y), px * py)
      expect_identical(x == y, px == py)
      expect_identical(x < y, px < py)
      expect_identical(
        unvecvec(vecvec_mapply(list(x, y), function(u, v) u - v)),
        px - py
      )
    }
  }
  # Repeated values are computed once, keeping the result compact
  r <- rep(vecvec(1:2, 0.5), 1000)
  expect_identical(sum(lengths(vv_slots(r + r))), 3L)
})
