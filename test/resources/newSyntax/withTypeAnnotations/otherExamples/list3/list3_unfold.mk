help y0 y1 y2 = (fresh q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12 in (((y2 == (Succ (Succ (Zero)) :: (Zero :: (Zero :: (Succ (Zero) :: (Succ (Succ (Zero)) :: (Zero :: (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: q1)))))))))) & y1 == (Succ (Succ (Zero)) :: (Zero :: (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))))))) & y0 == [] & reverso q1) | (y2 == (Succ (Succ (Zero)) :: (Zero :: (Zero :: (Succ (Zero) :: (Succ (Succ (Zero)) :: (Zero :: (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: q1)))))))))) & reverso q1 & y0 == (Succ (Succ (Zero)) :: q2) & appendo y1 q2))));

reverso y3 = y3 == [];

appendo y4 y5 = (fresh q1 in (((y5 == [] & y4 == (Zero :: (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))))))) | (y5 == (Zero :: q1) & _appendo y4 q1))));

_appendo y6 y7 = (fresh q1 in (((y7 == [] & y6 == (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))])))))))) | (y7 == (Succ (Zero) :: q1) & __appendo y6 q1))));

__appendo y8 y9 = (fresh q1 in (((y9 == [] & y8 == (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))))) | (y9 == (Succ (Zero) :: q1) & ___appendo y8 q1))));

___appendo y10 y11 = (fresh q1 in (((y11 == [] & y10 == (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))])))))) | (y11 == (Zero :: q1) & ____appendo y10 q1))));

____appendo y12 y13 = (fresh q1 in (((y13 == [] & y12 == (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))) | (y13 == (Succ (Succ (Zero)) :: q1) & _____appendo y12 q1))));

_____appendo y14 y15 = (fresh q1 in (((y15 == [] & y14 == (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))])))) | (y15 == (Succ (Zero) :: q1) & ______appendo y14 q1))));

______appendo y16 y17 = (fresh q1 in (((y17 == [] & y16 == (Zero :: (Zero :: [Succ (Succ (Zero))]))) | (y17 == (Zero :: q1) & _______appendo y16 q1))));

_______appendo y18 y19 = (fresh q1 in (((y19 == [] & y18 == (Zero :: [Succ (Succ (Zero))])) | (y19 == (Zero :: q1) & ________appendo y18 q1))));

________appendo y20 y21 = ((y21 == [] & y20 == [Succ (Succ (Zero))]) | (y21 == [Succ (Succ (Zero))] & y20 == []));


? help x0 x1 x2