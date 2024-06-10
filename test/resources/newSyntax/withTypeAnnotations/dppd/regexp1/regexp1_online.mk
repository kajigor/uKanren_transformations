generate y0 y1 y2 y3 y4 = (fresh q1 in ((y4 == [] | (generateGenerate y0 y1 y2 y3 y4 q1 & generate y0 y1 y2 y3 q1))));

generateGenerate y5 y6 y7 y8 y9 y10 = (fresh q1 in (((y9 == (y5 :: q1) & _generate y7 y8 y10 q1) | (y9 == (y6 :: q1) & _generate y7 y8 y10 q1))));

_generate y12 y13 y14 y15 = (y15 == (y12 :: y14) | y15 == (y13 :: y14));


? generate x0 x1 x2 x3 x4