applasto y0 = (fresh q1, q2 in (((y0 == (S (O) :: q1) & _appendo q1 []) | (y0 == (q2 :: q1) & appendoLasto q1))));

appendoLasto y1 = (fresh q1, q2, q3 in ((y1 == (q1 :: q2) & _appendo q2 q3 & lasto ((q1 :: q3)))));

_appendo y3 y4 = (fresh q1, q2, q3 in (((y4 == [O] & y3 == []) | (y4 == (q1 :: q2) & y3 == (q1 :: q3) & _appendo q3 q2))));

lasto y5 = (fresh q1, q2 in ((y5 == [S (O)] | (y5 == (q1 :: q2) & lasto q2))));


? applasto x0