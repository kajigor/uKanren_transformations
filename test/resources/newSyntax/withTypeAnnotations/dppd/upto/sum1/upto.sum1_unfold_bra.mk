sumsquaresupto y0 = uptoSquareSquaresSum y0;

uptoSquareSquaresSum y1 = (fresh q1, q2 in ((uptoSquareSquares (S (S (O))) q1 q2 & sum y1 (S (S (S (S (O))))) ((S (S (S (S (S (S (S (S (S (O))))))))) :: (q1 :: q2))))));

uptoSquareSquares y6 y9 y10 = (fresh q1, q2, q3 in (((y10 == (q1 :: q2) & uptoSquares q2 & y6 == O & square (S (S (O))) y9 & square (S (S (S (O)))) q1) | (y6 == S (q3) & leUptoSquareSquares y9 y10 q3))));

uptoSquares y12 = (fresh q1, q2 in ((y12 == [S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (O))))))))))))))))] | (y12 == (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (O)))))))))))))))) :: (q1 :: q2)) & uptoSquareSquares (S (S (S (O)))) q1 q2))));

upto y13 = (y13 == S (O) | (y13 == O & upto (S (O))));

squares y15 y16 = (fresh q1, q2, q3, q4 in (((y16 == [] & y15 == []) | (y16 == (q1 :: q2) & square q3 q1 & y15 == (q3 :: q4) & squares q4 q2))));

square y17 y18 = (fresh q1, q2 in (((y18 == O & y17 == O) | (y18 == S (q1) & y17 == S (q2) & _addMultiply q2 q1 q2 q2))));

_addMultiply y22 y24 y25 y26 = (fresh q1, q2 in (((y22 == O & multiply y24 y25 y26) | (y24 == S (q1) & y22 == S (q2) & _addMultiply q2 q1 y25 y26))));

multiply y27 y28 y29 = (fresh q1 in (((y29 == O & y27 == O) | (y29 == S (q1) & _addMultiply (S (y28)) y27 y28 q1))));

add y30 y31 y32 = (fresh q1, q2 in (((y31 == O & y30 == y32) | (y32 == S (q1) & y31 == S (q2) & add y30 q2 q1))));

leUptoSquareSquares y33 y34 y36 = (fresh q1, q2 in (((y36 == O & _uptoSquares q1 & y34 == (q2 :: q1) & square (S (S (S (O)))) y33 & square (S (S (S (S (O))))) q2) | (y36 == S (O) & _uptoSquares y34 & square (S (S (S (S (O))))) y33))));

_uptoSquares y38 = squares [] y38;

sum y39 y40 y41 = (fresh q1 in (((y40 == O & _sum1 y39 y41 (S (O))) | (y40 == S (q1) & _addSum1 y39 y41 q1))));

_sum1 y48 y49 y50 = (fresh q1, q2, q3 in (((y49 == [] & y48 == y50) | (y49 == (q1 :: q2) & add y50 q1 q3 & _sum1 y48 q2 q3))));

_addSum1 y51 y52 y53 = (fresh q1, q2 in (((y53 == O & _sum1 y51 y52 (S (S (O)))) | (y53 == S (q1) & add (S (O)) q1 q2 & _sum1 y51 y52 (S (S (q2)))))));


? sumsquaresupto x0