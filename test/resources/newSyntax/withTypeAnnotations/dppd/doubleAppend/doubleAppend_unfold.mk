double_appendo y0 y1 y2 = (fresh q1 in (((y0 == [] & appendo y2 y1) | (y0 == (S (O) :: q1) & appendoAppendo y1 y2 q1))));

appendo y3 y4 = (fresh q1 in (((y4 == [] & y3 == (S (O) :: (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))]))))))) | (y4 == (S (O) :: q1) & _appendo y3 q1))));

_appendo y5 y6 = (fresh q1 in (((y6 == [] & y5 == (S (S (O)) :: (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))])))))) | (y6 == (S (S (O)) :: q1) & __appendo y5 q1))));

__appendo y7 y8 = (fresh q1 in (((y8 == [] & y7 == (S (S (S (O))) :: (O :: (O :: (S (O) :: [S (S (O))]))))) | (y8 == (S (S (S (O))) :: q1) & ___appendo y7 q1))));

___appendo y9 y10 = (fresh q1 in (((y10 == [] & y9 == (O :: (O :: (S (O) :: [S (S (O))])))) | (y10 == (O :: q1) & ____appendo y9 q1))));

____appendo y11 y12 = (fresh q1 in (((y12 == [] & y11 == (O :: (S (O) :: [S (S (O))]))) | (y12 == (O :: q1) & _____appendo y11 q1))));

_____appendo y13 y14 = (fresh q1 in (((y14 == [] & y13 == (S (O) :: [S (S (O))])) | (y14 == (S (O) :: q1) & ______appendo y13 q1))));

______appendo y15 y16 = ((y16 == [] & y15 == [S (S (O))]) | (y16 == [S (S (O))] & y15 == []));

appendoAppendo y17 y18 y19 = (fresh q1 in (((y19 == [] & _appendo y18 y17) | (y19 == (S (S (O)) :: q1) & _appendoAppendo y17 y18 q1))));

_appendoAppendo y21 y22 y23 = (fresh q1 in (((y23 == [] & __appendo y22 y21) | (y23 == (S (S (S (O))) :: q1) & __appendoAppendo y21 y22 q1))));

__appendoAppendo y25 y26 y27 = (fresh q1 in (((y27 == [] & ___appendo y26 y25) | (y27 == (O :: q1) & ___appendoAppendo y25 y26 q1))));

___appendoAppendo y29 y30 y31 = (fresh q1 in (((y31 == [] & ____appendo y30 y29) | (y31 == (O :: q1) & ____appendoAppendo y29 y30 q1))));

____appendoAppendo y33 y34 y35 = (fresh q1 in (((y35 == [] & _____appendo y34 y33) | (y35 == (S (O) :: q1) & _____appendoAppendo y33 y34 q1))));

_____appendoAppendo y37 y38 y39 = (fresh q1 in (((y39 == [] & ______appendo y38 y37) | (y39 == (S (S (O)) :: q1) & y38 == [] & _______appendo y37 q1))));

_______appendo y41 y42 = (y42 == [] & y41 == []);


? double_appendo x0 x1 x2