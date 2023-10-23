nthOpt y0 y1 =
  fresh q1, q2 in
    (((y1 == None | y0 == []) & (y0 == (q1 :: q2) | nthOpt1 y1 q2)));

nthOpt1 y2 y3 =
  fresh q1, q2 in
    (((y3 == [] | y2 == None) & (y3 == (q1 :: q2) | nthOpt2 y2 q2)));

nthOpt2 y4 y5 =
  fresh q1, q2 in
    (((y5 == [] | y4 == None) & (y5 == (q1 :: q2) | nthOpt3 y4 q2)));

nthOpt3 y6 y7 =
  fresh q1, q2 in
    (((y7 == [] | y6 == None) & (y7 == (q1 :: q2) | y6 == Some q1)));

? nthOpt x0 x1