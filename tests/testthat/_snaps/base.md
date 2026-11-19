# models error for mixed-type vecvecs

    Code
      lm(y ~ x, df)
    Condition
      Error:
      ! Can't use `x` in a model: it is a mixed-type vecvec.
      i It holds values of 2 different types: <integer> and <character>.
      i Models need values of a single type. Convert it first, e.g. with `unvecvec()`.
    Code
      lm(y ~ x, model_data(x = vecvec(1:3, c(4.5, 5.5, 6.5))))
    Condition
      Error:
      ! Can't use `x` in a model: it is a mixed-type vecvec.
      i It holds values of 2 different types: <integer> and <double>.
      i Models need values of a single type. Convert it first, e.g. with `unvecvec()`.

# models error for types whose data isn't their value

    Code
      lm(y ~ x, df)
    Condition
      Error:
      ! Can't use `x` in a model: it is a vecvec of <factor<d0fd1>>.
      i Its data isn't the same as its values (see `vecvec::vecvec_storage_is_value()`).
      i Convert it first, e.g. with `as.numeric()` or `as.factor()`.

---

    Code
      lm(y ~ x, df)
    Condition
      Error:
      ! Can't use `x` in a model: it is a vecvec of <vctrs_rcrd>.
      i Its values aren't held in an atomic vector, so its data is an index.
      i Convert it first, e.g. with `unvecvec()`.

