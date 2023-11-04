sorto y0 = (fresh q1, q2, q3 in (((y0 == (q1 :: (q2 :: [q3])) & minmaxoMinmaxo q1 q2 q3) | (y0 == (q1 :: (q2 :: [q3])) & _minmaxoMinmaxo q1 q2 q3))));

minmaxoMinmaxo y1 y3 y4 = ((y1 == Zero & minmaxo y3 y4) | (y1 == Succ (Zero) & _minmaxo y3 y4));

minmaxo y5 y6 = ((y6 == Succ (Succ (Zero)) & y5 == Succ (Zero)) | (y6 == Succ (Zero) & y5 == Succ (Succ (Zero))));

_minmaxo y7 y8 = ((y8 == Succ (Succ (Zero)) & y7 == Zero) | (y8 == Zero & y7 == Succ (Succ (Zero))));

_minmaxoMinmaxo y9 y11 y12 = (y9 == Succ (Succ (Zero)) & ___minmaxo y11 y12);

___minmaxo y13 y14 = ((y14 == Succ (Zero) & y13 == Zero) | (y14 == Zero & y13 == Succ (Zero)));


? sorto x0