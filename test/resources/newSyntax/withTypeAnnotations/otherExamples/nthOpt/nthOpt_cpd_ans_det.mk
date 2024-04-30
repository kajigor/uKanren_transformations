nthOpt y0 y1 = (fresh q1, q2 in (((y1 == None & y0 == []) | (y0 == (q1 :: q2) & _nthOpt y1 q2))));

_nthOpt y2 y3 = (fresh q1, q2 in (((y3 == [] & y2 == None) | (y3 == (q1 :: q2) & __nthOpt y2 q2))));

__nthOpt y4 y5 = (fresh q1, q2 in (((y5 == [] & y4 == None) | (y5 == (q1 :: q2) & ___nthOpt y4 q2))));

___nthOpt y6 y7 = (fresh q1, q2 in (((y7 == [] & y6 == None) | (y7 == (q1 :: q2) & y6 == Some (q1)))));


? nthOpt x0 x1