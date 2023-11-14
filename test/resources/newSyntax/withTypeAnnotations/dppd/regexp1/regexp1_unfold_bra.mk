generate y0 y1 y2 y3 y4 =
 (fresh q1 in
  ((y4 == [] |
   (y4 == (y0 :: (y2 :: q1)) & generate y0 y1 y2 y3 q1) |
   (y4 == (y0 :: (y3 :: q1)) & generate y0 y1 y2 y3 q1) |
   (y4 == (y1 :: (y2 :: q1)) & generate y0 y1 y2 y3 q1) |
   (y4 == (y1 :: (y3 :: q1)) & generate y0 y1 y2 y3 q1))));


? generate x0 x1 x2 x3 x8