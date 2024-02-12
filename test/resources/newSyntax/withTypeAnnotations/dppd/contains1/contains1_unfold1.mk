containso y0 =
  (fresh q1, q2, q3, q4, q5, q6 in (((y0 == (S (O) :: (O :: (q2 :: q1))) & _cono q1) |
  (y0 == (S (O) :: (O :: (S (O) :: q1))) & __cono q1) | (y0 == (S (O) :: (q3 :: q4)) & _cono q4) |
  (y0 == (S (O) :: (S (O) :: q4)) & __cono q4) | (y0 == (q5 :: q6) & _cono q6))));

_cono y2 = (fresh q1, q2 in (((y2 == (S (O) :: q1) & __cono q1) | (y2 == (q2 :: q1) & _cono q1))));

__cono y3 =
  (fresh q1, q2, q3, q4 in (((y3 == (O :: (q2 :: q1)) & _cono q1) | (y3 == (O :: (S (O) :: q1)) & __cono q1) |
  (y3 == (q3 :: q4) & _cono q4) | (y3 == (S (O) :: q4) & __cono q4))));


? containso x0