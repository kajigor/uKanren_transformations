solve y0 y1 y2 = (fresh q1 in (((y0 == [] & my_clauseSolve y2 y1) | (y0 == (S (S (O)) :: q1) & solve_atomMy_clauseSolve q1 y1 y2))));

my_clauseSolve y3 y4 = (fresh q1 in (((y4 == [] & y3 == (S (S (O)) :: (S (O) :: (S (S (O)) :: [S (O)])))) | (y4 == (S (S (O)) :: q1) & _my_clauseSolve q1 y3))));

_my_clauseSolve y6 y7 = (fresh q1 in (((y7 == (S (O) :: (S (S (O)) :: [S (O)])) & y6 == []) | (y6 == (S (O) :: q1) & __my_clauseSolve q1 y7))));

__my_clauseSolve y9 y10 = (fresh q1 in (((y10 == (S (S (O)) :: [S (O)]) & y9 == []) | (y9 == (S (S (O)) :: q1) & ___my_clauseSolve q1 y10))));

___my_clauseSolve y12 y13 = ((y13 == [S (O)] & y12 == []) | (y13 == [] & y12 == [S (O)]));

solve_atomMy_clauseSolve y15 y16 y18 = (fresh q1 in (((y15 == [] & _my_clauseSolve y16 y18) | (y15 == (S (O) :: q1) & _solve_atomMy_clauseSolve q1 y16 y18))));

_solve_atomMy_clauseSolve y20 y21 y23 = (fresh q1 in (((y20 == [] & __my_clauseSolve y21 y23) | (y20 == (S (S (O)) :: q1) & __solve_atomMy_clauseSolve q1 y21 y23))));

__solve_atomMy_clauseSolve y25 y26 y28 = (fresh q1 in (((y25 == [] & ___my_clauseSolve y26 y28) | (y28 == [] & y25 == (S (O) :: q1) & solve_atom q1 y26))));

solve_atom y30 y31 = (y31 == [] & y30 == []);


? solve x0 x1 x2