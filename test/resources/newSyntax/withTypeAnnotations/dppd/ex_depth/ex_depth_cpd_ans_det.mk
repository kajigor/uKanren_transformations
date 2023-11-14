solve y0 y1 = (fresh q1, q2, q3, q4, q5, q6, q7 in (((y0 == (S (S (O)) :: (S (O) :: q1)) & clausSolve1Solve1Solve1 q2 q3 y1 & clausClaus q2 q1 q3) | (y0 == (S (S (O)) :: (S (O) :: q4)) & clausSolve1Solve1Solve1Solve1 q5 q6 q7 y1 & clausClaus q6 q4 q7 & claus (Member (O) ((O :: (S (O) :: (S (S (O)) :: [O]))))) q5))));

clausSolve1Solve1Solve1 y4 y6 y7 = (__solve1Solve1 (S (S (S (S (S (S (O))))))) y4 y6 y7 | _clausSolve1Solve1Solve1 y4 y6 y7);

claus y12 y13 = (fresh q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22, q23, q24 in (((y13 == [] & y12 == Member (q1) ((q1 :: q2))) | (y13 == [Member (q3) (q4)] & y12 == Member (q3) ((q5 :: q4))) | (y13 == (Member (q6) (q7) :: [Member (q6) (q8)]) & y12 == InBoth (q6) (q7) (q8)) | (y13 == [] & y12 == App ([]) (q9) (q9)) | (y13 == [App (q10) (q11) (q12)] & y12 == App ((q13 :: q10)) (q11) ((q13 :: q12))) | (y13 == [] & y12 == Delete (q14) ((q14 :: q15)) (q15)) | (y13 == [Delete (q16) (q17) (q18)] & y12 == Delete (q16) ((q19 :: q17)) ((q19 :: q18))) | (y13 == (InBoth (q20) (q21) (q22) :: (Delete (q20) (q21) (q23) :: [App (q23) (q22) (q24)])) & y12 == Test (q20) (q21) (q22) (q24)))));

solve1 y14 y15 y16 = (fresh q1, q2, q3, q4 in (((y15 == [] & y14 == y16) | (y15 == (q1 :: q2) & claus q1 q3 & solve1 q4 q3 (S (y16)) & solve1 y14 q2 q4))));

_clausSolve1Solve1Solve1 y17 y19 y20 = __solve1Solve1 (S (S (S (S (S (S (S (S (S (O)))))))))) y17 y19 y20;

clausClaus y27 y29 y30 = (fresh q1 in (((y27 == [] & claus (App ((O :: (S (S (O)) :: [S (O)]))) ((O :: (S (O) :: (S (S (O)) :: [O])))) (y29)) y30) | (y27 == [Delete (O) ((O :: (S (S (O)) :: [S (O)]))) (q1)] & claus (App ((O :: q1)) ((O :: (S (O) :: (S (S (O)) :: [O])))) (y29)) y30))));

clausSolve1Solve1Solve1Solve1 y33 y35 y37 y38 = solve1Solve1Solve1 y33 y35 y37 y38;

solve1Solve1Solve1 y39 y41 y43 y44 = (fresh q1, q2, q3, q4, q5 in (((y39 == [] & __solve1Solve1 (S (S (S (S (S (S (S (O)))))))) y41 y43 y44) | (y39 == (q1 :: q2) & __solve1Solve1 q3 y41 y43 y44 & claus q1 q4 & solve1 q5 q4 (S (S (S (S (S (S (S (S (O))))))))) & solve1 q3 q2 q5))));

__solve1Solve1 y45 y46 y48 y49 = (fresh q1, q2, q3, q4, q5 in (((y46 == [] & solve1 y49 y48 (S (S (S (S (S (S (y45)))))))) | (y46 == (q1 :: q2) & claus q1 q3 & solve1 q4 q3 (S (S (S (S (y45))))) & solve1 q5 q2 q4 & solve1 y49 y48 (S (S (S (q5))))))));


? solve x0 x1