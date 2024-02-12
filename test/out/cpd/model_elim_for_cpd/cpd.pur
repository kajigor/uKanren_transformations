solve y0 y1 y2 = contrapositiveProveall y0 y1 y2;

contrapositiveProveall y3 y4 y5 = (fresh q1, q2, q3 in (input_clauseProveall y3 y4 y5));

input_clauseProveall y7 y8 y9 = (fresh q1, q2, q3 in (((y8 == y9 & y7 == []) | (y9 == (q1 :: q2) & y7 == (q1 :: q3) & prove q3 y8 q2))));

prove y11 y12 y13 = contrapositiveProveall y11 y12 y13;


? solve x0 x1 x2