matcho y0 = (appendo y0 | appendoAppendo y0);

appendo y2 = (fresh q1 in ((y2 == [] | (y2 == (S (S (O)) :: q1) & _appendo q1))));

_appendo y4 = (fresh q1 in ((y4 == [] | (y4 == (S (O) :: q1) & __appendo q1))));

__appendo y6 = (fresh q1 in ((y6 == [] | (y6 == (S (O) :: q1) & ___appendo q1))));

___appendo y8 = (fresh q1 in ((y8 == [] | (y8 == (O :: q1) & ____appendo q1))));

____appendo y10 = (fresh q1 in ((y10 == [] | (y10 == (O :: q1) & _____appendo q1))));

_____appendo y12 = (fresh q1 in ((y12 == [] | (y12 == (S (S (O)) :: q1) & ______appendo q1))));

______appendo y14 = (fresh q1 in ((y14 == [] | (y14 == (O :: q1) & _______appendo q1))));

_______appendo y16 = (y16 == [] | y16 == [S (O)]);

appendoAppendo y17 = (_appendo y17 | _appendoAppendo y17);

_appendoAppendo y21 = (__appendo y21 | __appendoAppendo y21);

__appendoAppendo y25 = (___appendo y25 | ___appendoAppendo y25);

___appendoAppendo y29 = (____appendo y29 | ____appendoAppendo y29);

____appendoAppendo y33 = (_____appendo y33 | _____appendoAppendo y33);

_____appendoAppendo y37 = (______appendo y37 | ______appendoAppendo y37);

______appendoAppendo y41 = (_______appendo y41 | ________appendo y41);

________appendo y45 = y45 == [];


? matcho x0