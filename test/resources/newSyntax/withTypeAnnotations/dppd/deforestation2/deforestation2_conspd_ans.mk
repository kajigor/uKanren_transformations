rr y0 =
  fresh q1 in
    ((r q1 & r1 y0 q1));

r y1 =
  fresh q1 in
    ((y1 == [S O, S O, O, O, S (S O), S (S O), O, O, O, O]) | (y1 == (S O :: O :: O :: q1) & r2 q1));

r1 y2 y3 =
  fresh q1, q2, q3, q4, q5, q6, q7 in
    (((y3 == Nil & y2 == Nil) |
      (y3 == (q1 :: q2) & y2 == (q1 :: q1 :: q3) & r1 q3 q2) |
       (y3 == (q4 :: q5) & y2 == (q4 :: q6 :: q7) & neq q4 q6 & r1 ((q6 :: q7)) q5)));

r2 y4 =
  y4 == [S (S O), S (S O), O, O, O, O];

neq y5 y6 =
  fresh q1, q2, q3, q4 in
   (((y6 == S q1 & y5 == O) |
      (y6 == O & y5 == S q2) |
      (y6 == S q3 & y5 == S q4 & neq q4 q3)));

? rr x0