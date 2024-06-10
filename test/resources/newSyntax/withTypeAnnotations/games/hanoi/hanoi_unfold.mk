check y0 = (fresh q1 in (((y0 == (Pair (One) (Two) :: q1) & _check q1) | (y0 == (Pair (One) (Thr) :: q1) & __________________________check q1))));

_check y1 = (fresh q1 in (((y1 == (Pair (One) (Thr) :: q1) & __check q1) | (y1 == (Pair (Two) (Thr) :: q1) & __________________________check q1) | (y1 == (Pair (Two) (One) :: q1) & check q1))));

__check y2 = (fresh q1 in (((y2 == (Pair (Two) (One) :: q1) & ___check q1) | (y2 == (Pair (Two) (Thr) :: q1) & _________________________check q1) | (y2 == (Pair (Thr) (One) :: q1) & _check q1))));

___check y3 = (fresh q1 in (((y3 == (Pair (One) (Two) :: q1) & __check q1) | (y3 == (Pair (Thr) (Two) :: q1) & ____check q1) | (y3 == (Pair (One) (Thr) :: q1) & _________________________check q1))));

____check y4 = (fresh q1 in (((y4 == (Pair (One) (Thr) :: q1) & _____check q1) | (y4 == (Pair (Two) (Thr) :: q1) & ___check q1) | (y4 == (Pair (One) (Two) :: q1) & ______check q1))));

_____check y5 = (fresh q1 in (((y5 == (Pair (Two) (One) :: q1) & __________________________check q1) | (y5 == (Pair (Thr) (One) :: q1) & ____check q1) | (y5 == (Pair (Thr) (Two) :: q1) & ______check q1))));

______check y6 = (fresh q1 in (((y6 == (Pair (One) (Thr) :: q1) & _______check q1) | (y6 == (Pair (Two) (Thr) :: q1) & _____check q1) | (y6 == (Pair (Two) (One) :: q1) & ____check q1))));

_______check y7 = (fresh q1 in (((y7 == (Pair (Two) (One) :: q1) & ________check q1) | (y7 == (Pair (Thr) (One) :: q1) & ______check q1) | (y7 == (Pair (Two) (Thr) :: q1) & ________________________check q1))));

________check y8 = (fresh q1 in (((y8 == (Pair (One) (Two) :: q1) & _______check q1) | (y8 == (Pair (One) (Thr) :: q1) & ________________________check q1) | (y8 == (Pair (Two) (Thr) :: q1) & _________check q1))));

_________check y9 = (fresh q1 in (((y9 == (Pair (One) (Two) :: q1) & __________check q1) | (y9 == (Pair (Thr) (Two) :: q1) & ________check q1) | (y9 == (Pair (One) (Thr) :: q1) & _______________________check q1))));

__________check y10 = (fresh q1 in (((y10 == (Pair (Two) (One) :: q1) & _________check q1) | (y10 == (Pair (Thr) (One) :: q1) & ___________check q1) | (y10 == (Pair (Two) (Thr) :: q1) & _______________________check q1))));

___________check y11 = (fresh q1 in (((y11 == (Pair (One) (Thr) :: q1) & __________check q1) | (y11 == (Pair (Two) (One) :: q1) & ____________check q1) | (y11 == (Pair (Two) (Thr) :: q1) & ______________________check q1))));

____________check y12 = (fresh q1 in (((y12 == (Pair (One) (Two) :: q1) & ___________check q1) | (y12 == (Pair (Thr) (Two) :: q1) & _____________check q1) | (y12 == (Pair (One) (Thr) :: q1) & ______________________check q1))));

_____________check y13 = (fresh q1 in (((y13 == (Pair (One) (Thr) :: q1) & ______________check q1) | (y13 == (Pair (Two) (Thr) :: q1) & ____________check q1) | (y13 == (Pair (One) (Two) :: q1) & _____________________check q1))));

______________check y14 = (fresh q1 in (((y14 == (Pair (One) (Two) :: q1) & _______________check q1) | (y14 == (Pair (Thr) (One) :: q1) & _____________check q1) | (y14 == (Pair (Thr) (Two) :: q1) & _____________________check q1))));

_______________check y15 = (fresh q1 in (((y15 == (Pair (Two) (One) :: q1) & ______________check q1) | (y15 == (Pair (Thr) (One) :: q1) & ________________check q1) | (y15 == (Pair (Thr) (Two) :: q1) & ____________________check q1))));

________________check y16 = (fresh q1 in (((y16 == (Pair (One) (Thr) :: q1) & _______________check q1) | (y16 == (Pair (Two) (Thr) :: q1) & _________________check q1) | (y16 == (Pair (One) (Two) :: q1) & ____________________check q1))));

_________________check y17 = (fresh q1 in (((y17 == (Pair (One) (Two) :: q1) & __________________check q1) | (y17 == (Pair (One) (Thr) :: q1) & ___________________check q1) | (y17 == (Pair (Thr) (Two) :: q1) & ________________check q1))));

__________________check y18 = (fresh q1 in (((y18 == (Pair (Two) (One) :: q1) & _________________check q1) | (y18 == (Pair (Thr) (One) :: q1) & _____________________check q1) | (y18 == (Pair (Two) (Thr) :: q1) & ___________________check q1))));

___________________check y19 = (fresh q1 in (((y19 == (Pair (Two) (One) :: q1) & _________________________check q1) | (y19 == (Pair (Thr) (One) :: q1) & _________________check q1) | (y19 == (Pair (Thr) (Two) :: q1) & __________________check q1))));

____________________check y20 = (fresh q1 in (((y20 == (Pair (Two) (One) :: q1) & ________________check q1) | (y20 == (Pair (Two) (Thr) :: q1) & _______________check q1))));

_____________________check y21 = (fresh q1 in (((y21 == (Pair (One) (Thr) :: q1) & __________________check q1) | (y21 == (Pair (Two) (Thr) :: q1) & ______________check q1) | (y21 == (Pair (Two) (One) :: q1) & _____________check q1))));

______________________check y22 = (fresh q1 in (((y22 == (Pair (One) (Two) :: q1) & ________________________check q1) | (y22 == (Pair (Thr) (Two) :: q1) & ___________check q1) | (y22 == (Pair (Thr) (One) :: q1) & ____________check q1))));

_______________________check y23 = (fresh q1 in ((y23 == [] | (y23 == (Pair (Thr) (One) :: q1) & _________check q1) | (y23 == (Pair (Thr) (Two) :: q1) & __________check q1))));

________________________check y24 = (fresh q1 in (((y24 == (Pair (Two) (One) :: q1) & ______________________check q1) | (y24 == (Pair (Thr) (One) :: q1) & ________check q1) | (y24 == (Pair (Thr) (Two) :: q1) & _______check q1))));

_________________________check y25 = (fresh q1 in (((y25 == (Pair (One) (Two) :: q1) & ___________________check q1) | (y25 == (Pair (Thr) (Two) :: q1) & __check q1) | (y25 == (Pair (Thr) (One) :: q1) & ___check q1))));

__________________________check y26 = (fresh q1 in (((y26 == (Pair (One) (Two) :: q1) & _____check q1) | (y26 == (Pair (Thr) (Two) :: q1) & _check q1) | (y26 == (Pair (Thr) (One) :: q1) & check q1))));


? check x0