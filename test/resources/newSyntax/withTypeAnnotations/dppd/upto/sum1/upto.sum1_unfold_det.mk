sumsquaresupto y0 = _uptoMultiplySquaresAddSum1 y0 O O;

_uptoMultiplySquaresAddSum1 y7 y8 y13 = (fresh q1 in (((y8 == O & uptoSquaresSum1 y7 y13) | (y8 == S (q1) & leUptoAddAddAddMultiplySquaresAddSum1 y7 y13 q1))));

uptoSquaresSum1 y15 y16 = (y15 == S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (y16)))))))))))))))))))))))))))))) | _uptoMultiplySquaresAddSum1 y15 (S (S (S (O)))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (y16)))))))))))))))))))))))))))))));

leUptoAddAddAddMultiplySquaresAddSum1 y19 y21 y25 = ((y25 == O & _uptoSquaresSum1 y19 (S (S (S (S (S (S (S (S (S (y21))))))))))) | (y25 == S (O) & _uptoSquaresSum1 y19 y21));

_uptoSquaresSum1 y29 y31 = y29 == S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (y31)))))))))))))))));


? sumsquaresupto x0