sorto y0 = (fresh q1, q2, q3, q4, q5 in ((y0 == (Zero :: q1) & sortoAppendoAppendo q2 q3 q1 & __sorto q4 q3 & __sorto q5 q2)));

sortoAppendoAppendo y1 y2 y6 = (fresh q1, q2, q3, q4, q5, q6, q7, q8 in (((y6 == (Zero :: q1) & _appendo q1 (Succ (Zero)) y2 y1) | (splitoAppendo q2 q3 q4 q5 q6 q7 & appendoAppendo y1 y2 q2 y6 & __sorto q5 q6 & __sorto q8 q7))));

appendoAppendo y10 y11 y12 y14 = (fresh q1, q2, q3 in (((y12 == [] & _appendo y14 (Succ (Zero)) ((Zero :: y11)) y10) | (y14 == (q1 :: q2) & y12 == (q1 :: q3) & appendoAppendo y10 y11 q3 q2))));

splitoAppendo y15 y16 y17 y18 y20 y21 = (fresh q1, q2, q3 in (((y18 == [] & y17 == [] & _appendo y15 y16 y20 y21) | (y18 == (q1 :: q2) & splitoAppendo y15 y16 q3 q2 y20 y21 & y17 == (q1 :: q3) & le y16 q1) | (y17 == (q1 :: q3) & splitoAppendo y15 y16 q3 y18 y20 y21 & gt y16 q1))));

_appendo y22 y23 y24 y25 = (fresh q1, q2, q3 in (((y24 == [] & y22 == (y23 :: y25)) | (y24 == (q1 :: q2) & y22 == (q1 :: q3) & _appendo q3 y23 q2 y25))));

le y26 y27 = (fresh q1, q2 in ((y27 == Zero | (y27 == Succ (q1) & y26 == Succ (q2) & le q2 q1))));

gt y28 y29 = (fresh q1, q2, q3 in (((y29 == Succ (q1) & y28 == Zero) | (y29 == Succ (q2) & y28 == Succ (q3) & gt q3 q2))));

__sorto y32 y33 = (fresh q1, q2, q3, q4, q5, q6 in (((y33 == [] & y32 == []) | (y32 == (q1 :: q2) & splitoAppendo y33 q1 q2 q3 q4 q5 & __sorto q3 q4 & __sorto q6 q5))));


? sorto x0