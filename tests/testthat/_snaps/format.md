# print() on empty vecvec produces no output beyond the header

    Code
      print(vecvec())
    Output
      <vecvec[0]>

# print() on non-empty vecvec produces expected output

    Code
      print(vecvec(1:3, letters[1:3]))
    Output
      <vecvec[6]>
      [1] 1 2 3 a b c

