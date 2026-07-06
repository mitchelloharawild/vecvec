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

# print() on vecvec vector respects max and shows footer

    Code
      print(vecvec(1:10), max = 4L)
    Output
      <integer*[10]>
      [1] 1 2 3 4
      [ reached 'max' / getOption("max.print") -- omitted 6 entries ]

# print() on 2D vecvec matrix respects max (1 complete row shown)

    Code
      print(x, max = 4L)
    Output
      <integer*[3,3]>
           [,1] [,2] [,3]
      [1,] 1    4    7   
       [ reached 'max' / getOption("max.print") -- omitted 2 rows ]

# print() on 3D vecvec array respects max (identical structure to base array)

    Code
      print(x, max = 6L)
    Output
      <integer*[3,3,2]>
      , , 1
      
           [,1] [,2] [,3]
      [1,] 1    4    7   
      [2,] 2    5    8   
      
       [ reached 'max' / getOption("max.print") -- omitted 1 slice ] 

# print() on 3D vecvec array without truncation shows all slices

    Code
      print(x, max = 999L)
    Output
      <integer*[3,3,2]>
      , , 1
      
           [,1] [,2] [,3]
      [1,]  1    4    7  
      [2,]  2    5    8  
      [3,]  3    6    9  
      
      , , 2
      
           [,1] [,2] [,3]
      [1,] 10   13   16  
      [2,] 11   14   17  
      [3,] 12   15   18  
      

