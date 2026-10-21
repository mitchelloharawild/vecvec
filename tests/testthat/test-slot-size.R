# Slots may be any vctrs vector, so their size is vec_size() (rows), not
# length() (cells for a matrix, columns for a data frame).

test_that("matrix slots are indexed by row", {
  m <- vecvec(matrix(1:6, 3), matrix(c(7.5, 8.5), 1))
  full <- rbind(matrix(as.double(1:6), 3), c(7.5, 8.5))

  expect_equal(length(m), 4L)
  expect_equal(unvecvec(m), full)
  expect_equal(unvecvec(m[c(4, 2)]), full[c(4, 2), ])
  expect_equal(unvecvec(rev(m)), full[4:1, ])
  expect_equal(unvecvec(c(m, m)), rbind(full, full))
  expect_equal(unvecvec(vecvec_apply(m, function(x) x * 2)), full * 2)

  m[2] <- NA
  full[2, ] <- NA
  expect_equal(unvecvec(m), full)
})

test_that("data frame slots are indexed by row", {
  d <- vecvec(data.frame(a = 1:2), data.frame(a = 3L))

  expect_equal(length(d), 3L)
  expect_equal(unvecvec(d[3:1]), data.frame(a = 3:1))
})

test_that("vecvec_mapply() accepts results sized by vec_size()", {
  v <- vecvec(1:2, c(10, 20))

  expect_equal(
    vecvec_mapply(list(v), function(a) cbind(a, a), SIMPLIFY = TRUE),
    cbind(c(1, 2, 10, 20), c(1, 2, 10, 20)),
    ignore_attr = TRUE
  )
  expect_equal(
    vecvec_mapply(list(v), function(a) data.frame(a = a), SIMPLIFY = TRUE),
    data.frame(a = c(1, 2, 10, 20))
  )
  expect_error(
    vecvec_mapply(list(v), function(a) cbind(a, a)[1, , drop = FALSE]),
    "same size"
  )
})
