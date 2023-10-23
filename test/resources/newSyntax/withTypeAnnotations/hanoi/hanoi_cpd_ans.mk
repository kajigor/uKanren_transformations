check y0 =
  fresh q1, q2 in
    ((y0 == (q1 :: q2) & one_stepCheck q1 q2));

one_stepCheck y1 y2 =
  fresh q1 in
    ((y1 == Pair One q1 & notEqStickGetSetCheck y2 q1));

notEqStickGetSetCheck y4 y6 =
  ((y6 == Two & check1 y4) | (y6 == Thr & check3 y4));

check1 y7 =
  fresh q1, q2 in
    ((y7 == (q1 :: q2) & one_stepCheck1 q1 q2));

one_stepCheck1 y8 y9 =
  fresh q1, q2, q3, q4, q5, q6, q7, q8 in
    (((y8 == Pair q1 Thr & notEqStickGetSetCheck1 y9 q1)));

notEqStickGetSetCheck1 y11 y12 =
  ((y12 == Two & check3 y11));

check3 y22 =
  fresh q1, q2 in
    ((y22 == (q1 :: q2) & one_stepCheck3 q1 q2));

one_stepCheck3 y23 y24 =
  fresh q1, q2, q3, q4, q5, q6, q7, q8 in
    (((y23 == Pair q1 Two & notEqStickGetSetCheck2 y24 q1)));

notEqStickGetSetCheck2 y26 y27 =
  ((y27 == Thr & check1 y26));

? check x0