check y0 =
  fresh q1, q2, q3 in
    ((y0 == (q1 :: q2) & one_step q1 q3 & check1 q2 q3));

check1 y3 y4 =
  fresh q1, q2, q3, q4 in
    (((y3 == [] & y4 == Triple [] [] q1) | (y3 == (q2 :: q3) & one_step1 y4 q2 q4 & check2 q3 q4)));

one_step y1 y2 =
  fresh q1, q2 in
    ((y1 == Pair q1 q2 & ((q2 == Thr & q1 == One &
      (y2 == Triple (Cons (S O) (Cons (S (S O)) Nil)) Nil (Cons O Nil)))
     | (q2 == Two & q1 == One & (y2 == Triple [S O, S (S O)] [O] [])))));


one_step1 y5 y6 y7 =
  fresh q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14 in
    (((y6 == Pair q1 q2 & ((q2 == Two & q1 == Thr & y5 == Triple q3 [] (Cons q4 q5) & set y7 q4 q3 q5) |
    (q2 == One & q1 == Thr & (y5 == Triple [] q6 (Cons q4 q5)) & set1 y7 q4 q6 q5) | (q2 == Thr & q1 == Two &
    y5 == Triple q3 (Cons q4 q5) [] & set2 y7 q4 q3 q5) | (q2 == One & q1 == Two & y7 == Triple [q4] q5 q7 &
    y5 == Triple [] (Cons q4 q5) q7) | (q2 == Thr & q1 == One & y7 == Triple q5 q6 [q4] & y5 == Triple (Cons q4 q5) q6 []) |
    (q2 == Two & q1 == One & y7 == Triple q5 [q4] q7 & y5 == Triple (Cons q4 q5) [] q7))) | (y6 == Pair q1 q2 &
    ((q2 == Two & q1 == Thr & y5 == Triple q8 (Cons q9 q10) (Cons q4 q5) & lessSet y7 q4 q8 q9 q10 q5) |
    (q2 == One & q1 == Thr & y5 == Triple (Cons q9 q10) q11 (Cons q4 q5) & lessSet1 y7 q4 q11 q9 q10 q5) |
    (q2 == Thr & q1 == Two & y5 == Triple q8 (Cons q4 q5) (Cons q9 q10) & lessSet2 y7 q4 q8 q9 q10 q5) |
    (q2 == One & q1 == Two & (y7 == Triple (Cons q4 (Cons q9 q10)) q5 q12) & (y5 == Triple (Cons q9 q10) (Cons q4 q5) q12) & less q4 q9) |
    (q2 == Thr & q1 == One & (y7 == Triple q5 q11 (Cons q4 (Cons q9 q10))) & (y5 == Triple (Cons q4 q5) q11 (Cons q9 q10)) & less q4 q9) |
    (q2 == Two & q1 == One & ((y7 == Triple q5 (Cons O (Cons (S q13) q10)) q12 & y5 == Triple (Cons O q5) (Cons (S q13) q10) q12) |
    (y7 == Triple q5 (Cons (S q14) (Cons (S q13) q10)) q12 & y5 == Triple (Cons (S q14) q5) (Cons (S q13) q10) q12 & less q14 q13)))))));

check2 y8 y9 =
  fresh q1, q2 in
    ((y8 == y8 | (one_step1 q1 q2 y9 & check2 y8 y9)));

set y10 y11 y12 y13 =
  y10 == Triple y12 [y11] y13;

set1 y14 y15 y16 y17 =
  y14 == Triple [y15] y16 y17;

set2 y18 y19 y20 y21 =
  y18 == Triple y20 y21 [y19];

lessSet y22 y23 y24 y25 y26 y27 =
  (y22 == Triple y24 (Cons y23 (Cons y25 y26)) y27 & less y23 y25);

lessSet1 y28 y29 y30 y31 y32 y33 =
  (y28 == Triple (Cons y29 (Cons y31 y32)) y30 y33 & less y29 y31);

lessSet2 y34 y35 y36 y37 y38 y39 =
  (y34 == Triple y36 y39 (Cons y35 (Cons y37 y38)) & less y35 y37);

less y40 y41 =
  fresh q1, q2 in
    (((y41 == S q1 & y40 == O) | (y41 == S q1 & y40 == S q2 & less q2 q1)));

? check x0