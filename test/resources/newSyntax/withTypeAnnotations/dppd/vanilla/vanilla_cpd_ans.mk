solve y0 y1 y2 =
  (my_clauseSolveMy_clauseSolve y0 y1 y2 | solve y0 y1 y2);

my_clauseSolveMy_clauseSolve y3 y4 y5 =
  (fresh q1, q2, q3 in
    (((y3 == [] & my_clauseSolve y4 y5 ((S (O) :: (S (O) :: (S (S (O)) :: [S (O)]))))) |
      (y3 == (q1 :: q2) & my_clauseSolve q2 y4 q3 & my_clauseSolve ((q1 :: q3)) y5 ((S (O) :: (S (O) :: (S (S (O)) :: [S (O)]))))) |
      my_clauseSolveMy_clauseSolve y3 y4 y5)));

my_clauseSolve y9 y10 y11 =
  (fresh q1, q2, q3 in
    (((y10 == y11 & y9 == []) |
      (y11 == (q1 :: q2) & y9 == (q1 :: q3) & my_clauseSolve q3 y10 q2) |
      my_clauseSolve y9 y10 y11)));


? solve x0 x1 x2