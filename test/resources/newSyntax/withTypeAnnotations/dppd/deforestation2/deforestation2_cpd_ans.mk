rr y0 = (rR y0 | neqRR y0);
rR y1 = rR14 y1;
rR14 y3 = rR15 y3;
rR15 y5 = rR16 y5;
rR16 y7 = r y7;
r y9 = fresh q1, q2, q3 in
  (((y9 == (S O :: S O :: q1) & rR13 q1) | ((y9 == (S O :: q2 :: q3)) & (neqR1 q2 q3))));
rR13 y10 = fresh q1, q2, q3 in (((y10 == (S O :: S O :: q1) & rR12 q1) | (y10 == (S O :: q2 :: q3) & neqR q2 q3)));
rR12 y11 = fresh q1 in ((y11 == (O :: O :: q1) & rR11 q1));
rR11 y12 = fresh q1 in ((y12 == (O :: O :: q1) & rR10 q1));
rR10 y13 = fresh q1 in ((y13 == (S (S O) :: S (S O) :: q1) & rR9 q1));
rR9 y14 = fresh q1 in ((y14 == (S (S O) :: S (S O) :: q1) & rR8 q1));
rR8 y15 = fresh q1 in ((y15 == (O :: O :: q1) & rR7 q1));
rR7 y16 = fresh q1 in ((y16 == (O :: O :: q1) & rR6 q1));
rR6 y17 = fresh q1 in ((y17 == (O :: O :: q1) & rR5 q1));
rR5 y18 = (y18 == ([O, O]));
neqR y19 y20 = fresh q1 in (((y19 == O & rR12 (O :: y20)) | ((y19 == S (S q1)) & rR12 (S (S q1) :: y20))));
neqR1 y21 y22 = fresh q1 in (((y21 == O & rR13 (O :: y22)) | ((y21 == S (S q1)) & rR13 (S (S q1) :: y22))));
neqRR y23 = rR4 y23;
rR4 y26 = rR3 y26;
rR3 y28 = rR2 y28;
rR2 y30 = rR1 y30;
rR1 y32 = rR13 y32;

? rr x0