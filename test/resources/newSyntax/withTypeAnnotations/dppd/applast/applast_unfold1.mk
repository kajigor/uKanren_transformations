applasto y0 y1 y2 = (fresh q1, q2 in (((y1 == y2 & y0 == []) | (y0 == (y2 :: q1) & _appendo y1 q1 []) | (y0 == (q2 :: q1) & appendoLasto y1 y2 q1))));

appendoLasto y3 y4 y5 = (fresh q1, q2, q3 in (((y5 == [] & lasto y4 [y3]) | (y5 == (q1 :: q2) & _appendo y3 q2 q3 & lasto y4 ((q1 :: q3))))));

_appendo y7 y8 y9 = (fresh q1, q2, q3 in (((y9 == [y7] & y8 == []) | (y9 == (q1 :: q2) & y8 == (q1 :: q3) & _appendo y7 q3 q2))));

lasto y10 y11 = (fresh q1, q2 in ((y11 == [y10] | (y11 == (q1 :: q2) & lasto y10 q2))));


? applasto x0 x1 x2