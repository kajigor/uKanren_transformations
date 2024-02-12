help y0 y1 y2 = (fresh q1, q2, q3, q4, q5, q6, q7, q8, q9, q10 in ((y2 == (Succ (Succ (Zero)) :: (q1 :: (q2 :: (q3 :: (q4 :: (q5 :: (q6 :: (q7 :: (q8 :: (q9 :: q10)))))))))) & appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReverso q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 & appendo y0 y1)));

appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReverso y3 y5 y7 y9 y11 y13 y15 y17 y19 y20 = (y3 == Zero & appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReverso y5 y7 y9 y11 y13 y15 y17 y19 y20);

appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReverso y22 y24 y26 y28 y30 y32 y34 y36 y37 = (y22 == Zero & appendoAppendoAppendoAppendoAppendoAppendoAppendoReverso y24 y26 y28 y30 y32 y34 y36 y37);

appendoAppendoAppendoAppendoAppendoAppendoAppendoReverso y39 y41 y43 y45 y47 y49 y51 y52 = (y39 == Succ (Zero) & appendoAppendoAppendoAppendoAppendoAppendoReverso y41 y43 y45 y47 y49 y51 y52);

appendoAppendoAppendoAppendoAppendoAppendoReverso y54 y56 y58 y60 y62 y64 y65 = (y54 == Succ (Succ (Zero)) & appendoAppendoAppendoAppendoAppendoReverso y56 y58 y60 y62 y64 y65);

appendoAppendoAppendoAppendoAppendoReverso y67 y69 y71 y73 y75 y76 = (y67 == Zero & appendoAppendoAppendoAppendoReverso y69 y71 y73 y75 y76);

appendoAppendoAppendoAppendoReverso y78 y80 y82 y84 y85 = (y78 == Succ (Zero) & appendoAppendoAppendoReverso y80 y82 y84 y85);

appendoAppendoAppendoReverso y87 y89 y91 y92 = (y87 == Succ (Zero) & appendoAppendoReverso y89 y91 y92);

appendoAppendoReverso y94 y96 y97 = (y94 == Zero & appendoReverso y96 y97);

appendoReverso y99 y100 = (y99 == Succ (Succ (Zero)) & reverso y100);

reverso y102 = y102 == [];

appendo y103 y104 = (fresh q1 in (((y104 == (Succ (Succ (Zero)) :: (Zero :: (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))))))) & y103 == []) | (y103 == (Succ (Succ (Zero)) :: q1) & _appendo y104 q1))));

_appendo y105 y106 = (fresh q1 in (((y106 == [] & y105 == (Zero :: (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))))))) | (y106 == (Zero :: q1) & __appendo y105 q1))));

__appendo y107 y108 = (fresh q1 in (((y108 == [] & y107 == (Succ (Zero) :: (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))])))))))) | (y108 == (Succ (Zero) :: q1) & ___appendo y107 q1))));

___appendo y109 y110 = (fresh q1 in (((y110 == [] & y109 == (Succ (Zero) :: (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))))) | (y110 == (Succ (Zero) :: q1) & ____appendo y109 q1))));

____appendo y111 y112 = (fresh q1 in (((y112 == [] & y111 == (Zero :: (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))])))))) | (y112 == (Zero :: q1) & _____appendo y111 q1))));

_____appendo y113 y114 = (fresh q1 in (((y114 == [] & y113 == (Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))]))))) | (y114 == (Succ (Succ (Zero)) :: q1) & ______appendo y113 q1))));

______appendo y115 y116 = (fresh q1 in (((y116 == [] & y115 == (Succ (Zero) :: (Zero :: (Zero :: [Succ (Succ (Zero))])))) | (y116 == (Succ (Zero) :: q1) & _______appendo y115 q1))));

_______appendo y117 y118 = (fresh q1 in (((y118 == [] & y117 == (Zero :: (Zero :: [Succ (Succ (Zero))]))) | (y118 == (Zero :: q1) & ________appendo y117 q1))));

________appendo y119 y120 = (fresh q1 in (((y120 == [] & y119 == (Zero :: [Succ (Succ (Zero))])) | (y120 == (Zero :: q1) & _________appendo y119 q1))));

_________appendo y121 y122 = ((y122 == [] & y121 == [Succ (Succ (Zero))]) | (y122 == [Succ (Succ (Zero))] & y121 == []));


? help x0 x1 x2