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
    (((y8 == Pair q1 Thr & notEqStickGetSetCheck1 y9 q1) | (y8 == Pair q1 q2 & notEqStickGetGetLessSetSetCheck1 y9 q3 q1 q2 q4 q5 q6 q7 q8)));

notEqStickGetSetCheck1 y11 y12 =
  ((y12 == One & check2 y11) | (y12 == Two & check3 y11));

check2 y18 =
  fresh q1, q2 in
    ((y18 == (q1 :: q2) & one_stepCheck2 q1 q2));

one_stepCheck2 y19 y20 =
  fresh q1, q2, q3, q4, q5, q6, q7, q8 in
    ((y19 == Pair q1 q2 & notEqStickGetGetLessSetSetCheck y20 q3 q1 q2 q4 q5 q6 q7 q8));

check3 y22 =
  fresh q1, q2 in
    ((y22 == (q1 :: q2) & one_stepCheck3 q1 q2));

one_stepCheck3 y23 y24 =
  fresh q1, q2, q3, q4, q5, q6, q7, q8 in
    (((y23 == Pair q1 Two & notEqStickGetSetCheck2 y24 q1) | (y23 == Pair q1 q2 & notEqStickGetGetLessSetSetCheck3 y24 q3 q1 q2 q4 q5 q6 q7 q8)));

notEqStickGetSetCheck2 y26 y27 =
  ((y27 == One & check4 y26) | (y27 == Thr & check1 y26));

check4 y33 =
  fresh q1, q2 in
    ((y33 == (q1 :: q2) & one_stepCheck4 q1 q2));

one_stepCheck4 y34 y35 =
  fresh q1, q2, q3, q4, q5, q6, q7, q8 in
    ((y34 == Pair q1 q2 & notEqStickGetGetLessSetSetCheck2 y35 q3 q1 q2 q4 q5 q6 q7 q8));

notEqStickGetGetLessSetSetCheck x1 x2 x3 x4 x5 x6 x7 x8 =
  notEqStickGetGetLessSetSetCheck x1 x2 x3 x4 x5 x6 x7 x8;
notEqStickGetGetLessSetSetCheck1 x1 x2 x3 x4 x5 x6 x7 x8 =
  notEqStickGetGetLessSetSetCheck1 x1 x2 x3 x4 x5 x6 x7 x8;
notEqStickGetGetLessSetSetCheck2 x1 x2 x3 x4 x5 x6 x7 x8 =
  notEqStickGetGetLessSetSetCheck2 x1 x2 x3 x4 x5 x6 x7 x8;
notEqStickGetGetLessSetSetCheck3 x1 x2 x3 x4 x5 x6 x7 x8 =
  notEqStickGetGetLessSetSetCheck3 x1 x2 x3 x4 x5 x6 x7 x8;

? check x0