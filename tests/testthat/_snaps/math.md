# cumulative functions work on a single type, as on the plain vector

    Code
      cumsum(vecvec(1:2, 0.5))
    Condition
      Error:
      ! `cumsum()` is not supported for mixed-type <vecvec> objects.
      i Call `unvecvec()` first if the types have a common type.

