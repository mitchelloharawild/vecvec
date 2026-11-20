# What base R (and C code) sees of a vecvec: the values of a single-type
# vecvec, and an index (that models refuse) for a mixed-type one.

# A copy as C code makes it: new data, then the attributes of the original.
c_copy <- function(x, i) {
  data <- unclass(x)
  attributes(data) <- NULL
  out <- data[i]
  attributes(out) <- attributes(x)[setdiff(names(attributes(x)), c("dim", "dimnames", "names"))]
  out
}

# Storage ----------------------------------------------------------------------

test_that("a single-type vecvec's data is its values", {
  x <- vecvec(c(1.5, 2.5), 3.5)
  expect_true(vv_is_typed(x))
  expect_identical(typeof(x), "double")
  expect_identical(as.vector(unclass(vv_data(x))), c(1.5, 2.5, 3.5))

  expect_identical(typeof(vecvec(letters)), "character")
  expect_identical(typeof(vecvec(1:3)), "integer")
  expect_identical(typeof(vecvec(Sys.Date())), "double")
})

test_that("a mixed-type vecvec's data is an index", {
  x <- vecvec(1:2, "a")
  expect_false(vv_is_typed(x))
  expect_identical(typeof(x), "integer")
  expect_identical(as.vector(vv_data(x)), 1:3)
  expect_identical(x@x, list(1:2, "a"))
})

test_that("a single-type vecvec keeps the attributes of its type", {
  d <- as.POSIXct("2020-01-01", tz = "UTC") + 0:2
  x <- vecvec(d)
  expect_identical(x@x, list(d))
  expect_identical(x[[2]], d[[2]])
  expect_identical(unvecvec(x), d)

  f <- factor(c("b", "a", "b"))
  expect_identical(unvecvec(vecvec(f)), f)
})

test_that("vecvecs are single-type exactly when their elements share a type", {
  x <- vecvec(1:3, "a", 4:5)
  expect_false(vv_is_typed(x))
  expect_true(vv_is_typed(x[-4]))
  expect_identical(unvecvec(x[-4]), 1:5)
  expect_false(vv_is_typed(c(x[-4], vecvec("b"))))

  # Types differ by their attributes too
  expect_false(vv_is_typed(vecvec(factor("a"), factor("b"))))
  expect_false(vv_is_typed(vecvec(1L, 2.5)))
})

test_that("missing values without a type don't make a vecvec mixed-type", {
  x <- vecvec(1:3, NA)
  expect_true(vv_is_typed(x))
  expect_identical(unvecvec(x), c(1:3, NA))

  y <- vecvec(1:3)
  y[2] <- NA
  expect_identical(unvecvec(y), c(1L, NA, 3L))
  expect_identical(unvecvec(c(y, vecvec(NA))), c(1L, NA, 3L, NA))

  expect_identical(unvecvec(vecvec(NA)), NA)
})

test_that("replacing stored vectors can change the form", {
  x <- vecvec(1:3)
  x@x <- list(as.character(1:3))
  expect_identical(typeof(x), "character")
  x@x <- list(vctrs::new_rcrd(list(a = 1:3)))
  expect_false(vv_is_typed(x))
  expect_identical(vec_size(x@x[[1L]]), 3L)
})

test_that("zero-length and all missing vecvecs keep their type", {
  expect_true(vv_is_typed(vecvec(1:3)[0]))
  expect_identical(unvecvec(vecvec(1:3)[0]), integer())
  expect_identical(unvecvec(vecvec(1:3)[NA_integer_]), NA_integer_)
  expect_identical(unvecvec(vec_init(vecvec(letters), 2L)), c(NA_character_, NA))
})

test_that("vctrs types are a single type, or any types", {
  # A single-type vecvec is a vector of its type
  expect_true(vv_is_typed(vec_ptype(vecvec(1:3))))
  expect_true(vec_is(vecvec(1:3), vec_ptype(vecvec(4L))))
  expect_false(vec_is(vecvec(1:3), vec_ptype(vecvec("a"))))
  # A mixed-type vecvec is a vector of any types
  expect_identical(vec_ptype(vecvec("a", 1)), vec_ptype(vecvec(TRUE, 2L)))
  # vec_is() needs the exact type (as vec_is(1L, double()) is FALSE)
  expect_false(vec_is(vecvec(1:3), vec_ptype(vecvec("a", 1))))
  expect_true(vec_is(vecvec(1L, "a"), vec_ptype(vecvec("a", 1))))

  # The common type is single-type only when every vecvec has that type
  expect_true(vv_is_typed(vec_ptype2(vecvec(1), vecvec(2))))
  expect_true(vv_is_typed(vec_ptype2(vecvec(1), 2)))
  expect_false(vv_is_typed(vec_ptype2(vecvec(1), vecvec(1L))))
  expect_false(vv_is_typed(vec_ptype2(vecvec(1), vecvec(1, "a"))))
  expect_false(vv_is_typed(vec_ptype2(vecvec(1), vecvec())))
})

test_that("casts give the vctrs type of the target", {
  # Values are cast to the type of a single-type target
  expect_identical(vec_cast(vecvec(1:2), vecvec(0.5)), vecvec(c(1, 2)))
  expect_error(vec_cast(vecvec("a"), vecvec(0.5)), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(vecvec(1.5), vecvec(1L)), class = "vctrs_error_cast_lossy")
  expect_identical(vec_cast(vecvec(1, "a")[1], vecvec(0.5)), vecvec(1))

  # and kept for a mixed-type target, where a single type is held as an index
  out <- vec_cast(vecvec(1:2), vecvec(1, "a"))
  expect_false(vv_is_typed(out))
  expect_identical(unvecvec(out), 1:2)
  # which any other operation makes single-type again
  expect_identical(out[1:2], vecvec(1:2))

  # vctrs assignment keeps the type of the vector assigned into, as for any
  # other type (base assignment mixes types)
  expect_error(vec_assign(vecvec(1:3), 2L, vecvec("a")), class = "vctrs_error_incompatible_type")
  x <- vecvec(1:3)
  x[2] <- "a"
  expect_false(vv_is_typed(x))
  expect_identical(vec_assign(vecvec(1, "a"), 1L, vecvec(TRUE))[1], vecvec(TRUE))
})

test_that("prototypes don't depend on the order their types were combined in", {
  a <- vecvec(1:3)
  b <- vecvec(letters)
  expect_identical(vec_ptype2(a, b), vec_ptype2(b, a))
})

test_that("mixed-type vecvecs only store values they use", {
  x <- vecvec(1:3, "a")
  expect_identical(x[0]@x, list())
  expect_identical(x[c(NA, NA)]@x, list())
  expect_identical(x[c(1, 4, 1)]@x, list(1L, "a"))
  expect_identical(vecvec(vctrs::new_rcrd(list(a = 1:2)))[0]@x, list())
})

test_that("dim, dimnames and names are kept on the data", {
  x <- array(vecvec(as.double(1:6)), dim = 2:3)
  expect_identical(dim(vv_data(x)), 2:3)
  expect_identical(unvecvec(t(x)[3, ]), c(5, 6))

  m <- array(vecvec(1:3, letters[1:3]), dim = 2:3)
  expect_identical(format(t(m)), t(format(m)))
})

# Base R modelling -------------------------------------------------------------

model_data <- function(...) {
  data.frame(y = c(1, 3, 5, 7, 9, 12), z = c(2, 1, 4, 3, 6, 5), ...)
}

test_that("lm() and glm() fit on the values of a numeric vecvec", {
  df <- model_data(x = vecvec(c(1, 2), c(3, 4, 5, 6)))
  dfp <- model_data(x = c(1, 2, 3, 4, 5, 6))
  expect_equal(coef(lm(y ~ x + z, df)), coef(lm(y ~ x + z, dfp)))
  expect_equal(coef(glm(y ~ x, data = df)), coef(glm(y ~ x, data = dfp)))
  expect_equal(model.matrix(~x, df), model.matrix(~x, dfp))
})

test_that("prcomp() and data.matrix() use the values of a numeric vecvec", {
  df <- model_data(x = vecvec(c(1, 2), c(3, 4, 5, 7)))
  dfp <- model_data(x = c(1, 2, 3, 4, 5, 7))
  expect_equal(prcomp(~ x + z, df)$sdev, prcomp(~ x + z, dfp)$sdev)
  expect_equal(prcomp(df)$sdev, prcomp(dfp)$sdev)
  expect_equal(data.matrix(df), data.matrix(dfp))
})

test_that("lm() treats character and logical vecvecs like character and logical vectors", {
  s <- c("a", "b", "a", "b", "c", "c")
  df <- model_data(x = vecvec(s))
  dfp <- model_data(x = s)
  expect_equal(coef(lm(y ~ x, df)), coef(lm(y ~ x, dfp)))

  l <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  expect_equal(
    coef(lm(y ~ x, model_data(x = vecvec(l)))),
    coef(lm(y ~ x, model_data(x = l)))
  )
})

test_that("lm() uses dates and date-times as numbers, as base R does", {
  d <- as.Date("2020-01-01") + c(0, 3, 5, 9, 10, 20)
  expect_equal(
    coef(lm(y ~ x, model_data(x = vecvec(d)))),
    coef(lm(y ~ x, model_data(x = d)))
  )
})

test_that("models error for mixed-type vecvecs", {
  df <- model_data(x = vecvec(1:3, letters[1:3]))
  expect_snapshot(error = TRUE, {
    lm(y ~ x, df)
    lm(y ~ x, model_data(x = vecvec(1:3, c(4.5, 5.5, 6.5))))
  })
  expect_error(glm(y ~ x, data = df), "mixed-type")
  expect_error(model.matrix(~x, df), "mixed-type")
  expect_error(prcomp(~ x + z, df), "mixed-type")
  expect_false(is.numeric(df$x))
  expect_error(prcomp(df))
})

test_that("models error for a single type held as an index", {
  df <- model_data(x = vecvec(vctrs::new_rcrd(list(a = 1:6))))
  expect_snapshot(error = TRUE, lm(y ~ x, df))
  expect_false(is.numeric(df$x))
})

test_that("models error for a single factor type", {
  df <- model_data(x = vecvec(factor(c("a", "b", "a", "c", "b", "a"))))
  expect_snapshot(error = TRUE, lm(y ~ x, df))
  df <- model_data(x = vecvec(factor(c("a", "b", "a", "c", "b", "a"), ordered = TRUE)))
  expect_error(lm(y ~ x, df), class = "rlang_error")
  expect_false(is.numeric(df$x))
})

test_that("models use the data of any single type, as for the plain vector", {
  # Whether the data of a classed type is sensible in a model is up to the type
  k <- structure(c(1, 2, 3, 4, 5, 7), class = "vv_test_class")
  df <- model_data(x = vecvec(k))
  expect_true(is.numeric(df$x))
  expect_equal(
    coef(lm(y ~ x, df)),
    coef(lm(y ~ x, model_data(x = c(1, 2, 3, 4, 5, 7))))
  )
  expect_false(is.numeric(vecvec(as.Date("2020-01-01"))))
})

# Copies made by C code ---------------------------------------------------------

test_that("a single-type vecvec stays valid when C code copies it", {
  x <- vecvec(c("a", "b", "c"))
  expect_identical(c_copy(x, c(3, 1, 1)), x[c(3, 1, 1)])

  x <- vecvec(factor(c("p", "q", "p")))
  expect_identical(unvecvec(c_copy(x, 3:1)), unvecvec(x)[3:1])
})

test_that("a mixed-type vecvec stays valid when C code copies it", {
  x <- vecvec(c("a", "b"), 3L)
  y <- c_copy(x, c(3, 1, 1))
  expect_identical(format(y), c("3", "a", "a"))
  expect_identical(y[[1]], 3L)
})

test_that("vecvecs survive data.table and tidyr", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("tidyr")
  for (v in list(vecvec(c("a", "b", "c")), vecvec(1:2, "c"), vecvec(c(1.5, 1.5, 2)))) {
    # (data.table's `[` isn't used: it falls back to data.frame's in packages
    # that don't import data.table)
    dt <- data.table::data.table(k = 3:1, v = v)
    expect_identical(format(data.table::rbindlist(list(dt, dt))$v), format(c(v, v)))
    data.table::setorder(dt, k)
    expect_identical(format(dt$v), format(v[3:1]))

    g <- tidyr::gather(data.frame(id = v, a = 1:3, b = 4:6), key, val, a, b)
    expect_identical(format(g$id), format(c(v, v)))
  }
})

test_that("serialization keeps vecvecs", {
  for (v in list(vecvec(letters), vecvec(1:2, "c"), vecvec(factor("a"))[c(1, 1)])) {
    r <- unserialize(serialize(v, NULL))
    expect_identical(r, v)
  }
})

test_that("identical() tells vecvecs apart", {
  expect_false(identical(vecvec("a", "b"), vecvec("b", "a")))
  expect_false(identical(vecvec(1L, "a"), vecvec("a", 1L)))
  expect_true(identical(vecvec(1:3, "a"), vecvec(1:3, "a")))
  expect_false(identical(vecvec(factor("a")), vecvec(factor("a", levels = c("a", "b")))))
})

test_that("writing into a copy of a vecvec's data doesn't change the vecvec", {
  x <- vecvec(c(1, 2, 3))
  y <- unclass(x)
  y[1] <- 100
  expect_identical(unvecvec(x), c(1, 2, 3))
})

test_that("time series are kept in mixed-type form", {
  x <- vecvec(ts(1:4))
  expect_false(vv_is_typed(x))
  expect_identical(x[[2]], 2L)
})

test_that("vecvecs saved by vecvec 1.4.0 can be read", {
  # The layout of vecvec 1.4.0: an integer index with the slots in `x`
  old <- structure(
    c(3L, 1L, 2L),
    x = list(c("a", "b"), 3.5),
    class = c("vecvec::vecvec", "integer", "S7_object"),
    S7_class = class_vecvec
  )
  expect_identical(format(old), c("3.5", "a", "b"))
  expect_identical(unvecvec(old, ptype = character()), c("3.5", "a", "b"))
  new <- old[2:3]
  expect_null(attr(new, "x"))
  expect_true(vv_is_typed(new))
  expect_identical(new@x, list(c("a", "b")))
  expect_identical(old[c(1, 2)]@x, list("a", 3.5))
})

test_that("a mixed-type vecvec whose index was replaced errors", {
  x <- vecvec(1:2, "c")
  y <- x
  attr(y, "vecvec_x") <- NULL
  expect_error(format(y), "stored vectors")
})

# Subclasses ---------------------------------------------------------------------

test_that("subclasses keep their class in both forms", {
  sub <- S7::new_class("vv_test_sub", parent = class_vecvec, package = NULL)
  x <- sub(list(1:3, "a"))
  expect_s3_class(x, "vv_test_sub")
  expect_s3_class(x[1:3], "vv_test_sub")
  expect_true(vv_is_typed(x[1:3]))
  expect_s3_class(c(x[1:3], x[4]), "vv_test_sub")
})

test_that("values of a type read back from a file are still of that type", {
  # Types like S7 classes hold functions, whose environments are new copies
  # when deserialised
  fn <- local(function() NULL)
  x <- vecvec(structure(c(1, 2), class = "vv_fn_type", fn = fn))
  y <- unserialize(serialize(x, NULL))
  expect_false(identical(attributes(vv_typed_ptype(x)), attributes(vv_typed_ptype(y))))
  expect_true(vv_is_typed(c(x, y)))
  expect_true(vv_is_typed(vctrs::vec_c(x, y)))
  expect_true(vv_is_typed(vctrs::vec_ptype2(x, y)))
})
