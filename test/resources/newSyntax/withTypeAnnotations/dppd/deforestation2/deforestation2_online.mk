rr y0 = (fresh q1, q2, q3, q4, q5 in (((y0 == (q1 :: (q1 :: q2)) & rR q1 q2) | (y0 == (q3 :: (q4 :: q5)) & neqRR q3 q4 q5))));

rR y1 y2 = (fresh q1, q2, q3, q4, q5, q6, q7 in (((y2 == (q1 :: (q1 :: q2)) & r q2 q3 & r ((y1 :: (q1 :: q3))) ((S (O) :: (O :: (S (S (O)) :: (O :: [O])))))) | (y2 == (q4 :: (q5 :: q6)) & neq q4 q5 & r ((q5 :: q6)) q7 & r ((y1 :: (q4 :: q7))) ((S (O) :: (O :: (S (S (O)) :: (O :: [O])))))))));

r y4 y5 = (fresh q1, q2, q3, q4, q5, q6, q7 in (((y5 == [] & y4 == []) | (y5 == (q1 :: q2) & y4 == (q1 :: (q1 :: q3)) & r q3 q2) | (y5 == (q4 :: q5) & neq q4 q6 & y4 == (q4 :: (q6 :: q7)) & r ((q6 :: q7)) q5))));

neq y6 y7 = (fresh q1, q2, q3, q4 in (((y7 == S (q1) & y6 == O) | (y7 == O & y6 == S (q2)) | (y7 == S (q3) & y6 == S (q4) & neq q4 q3))));

neqRR y8 y9 y10 = (fresh q1, q2, q3, q4 in (((y9 == S (q1) & y8 == O & rR O ((S (q1) :: y10))) | (y9 == O & y8 == S (q2) & rR (S (q2)) ((O :: y10))) | (y9 == S (q3) & rR (S (q4)) ((S (q3) :: y10)) & y8 == S (q4) & neq q4 q3))));


? rr x0