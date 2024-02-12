check y0 = (fresh q1 in (((y0 == ((One, Two) :: q1) & _check q1) | (y0 == ((One, Thr) :: q1) & __________________________check q1))));

_check y1 = (fresh q1 in (((y1 == ((One, Thr) :: q1) & __check q1) | (y1 == ((Two, Thr) :: q1) & __________________________check q1) | (y1 == ((Two, One) :: q1) & check q1))));

__check y2 = (fresh q1 in (((y2 == ((Two, One) :: q1) & ___check q1) | (y2 == ((Two, Thr) :: q1) & _________________________check q1) | (y2 == ((Thr, One) :: q1) & _check q1))));

___check y3 = (fresh q1 in (((y3 == ((One, Two) :: q1) & __check q1) | (y3 == ((Thr, Two) :: q1) & ____check q1) | (y3 == ((One, Thr) :: q1) & _________________________check q1))));

____check y4 = (fresh q1 in (((y4 == ((One, Thr) :: q1) & _____check q1) | (y4 == ((Two, Thr) :: q1) & ___check q1) | (y4 == ((One, Two) :: q1) & ______check q1))));

_____check y5 = (fresh q1 in (((y5 == ((Two, One) :: q1) & __________________________check q1) | (y5 == ((Thr, One) :: q1) & ____check q1) | (y5 == ((Thr, Two) :: q1) & ______check q1))));

______check y6 = (fresh q1 in (((y6 == ((One, Thr) :: q1) & _______check q1) | (y6 == ((Two, Thr) :: q1) & _____check q1) | (y6 == ((Two, One) :: q1) & ____check q1))));

_______check y7 = (fresh q1 in (((y7 == ((Two, One) :: q1) & ________check q1) | (y7 == ((Thr, One) :: q1) & ______check q1) | (y7 == ((Two, Thr) :: q1) & ________________________check q1))));

________check y8 = (fresh q1 in (((y8 == ((One, Two) :: q1) & _______check q1) | (y8 == ((One, Thr) :: q1) & ________________________check q1) | (y8 == ((Two, Thr) :: q1) & _________check q1))));

_________check y9 = (fresh q1 in (((y9 == ((One, Two) :: q1) & __________check q1) | (y9 == ((Thr, Two) :: q1) & ________check q1) | (y9 == ((One, Thr) :: q1) & _______________________check q1))));

__________check y10 = (fresh q1 in (((y10 == ((Two, One) :: q1) & _________check q1) | (y10 == ((Thr, One) :: q1) & ___________check q1) | (y10 == ((Two, Thr) :: q1) & _______________________check q1))));

___________check y11 = (fresh q1 in (((y11 == ((One, Thr) :: q1) & __________check q1) | (y11 == ((Two, One) :: q1) & ____________check q1) | (y11 == ((Two, Thr) :: q1) & ______________________check q1))));

____________check y12 = (fresh q1 in (((y12 == ((One, Two) :: q1) & ___________check q1) | (y12 == ((Thr, Two) :: q1) & _____________check q1) | (y12 == ((One, Thr) :: q1) & ______________________check q1))));

_____________check y13 = (fresh q1 in (((y13 == ((One, Thr) :: q1) & ______________check q1) | (y13 == ((Two, Thr) :: q1) & ____________check q1) | (y13 == ((One, Two) :: q1) & _____________________check q1))));

______________check y14 = (fresh q1 in (((y14 == ((One, Two) :: q1) & _______________check q1) | (y14 == ((Thr, One) :: q1) & _____________check q1) | (y14 == ((Thr, Two) :: q1) & _____________________check q1))));

_______________check y15 = (fresh q1 in (((y15 == ((Two, One) :: q1) & ______________check q1) | (y15 == ((Thr, One) :: q1) & ________________check q1) | (y15 == ((Thr, Two) :: q1) & ____________________check q1))));

________________check y16 = (fresh q1 in (((y16 == ((One, Thr) :: q1) & _______________check q1) | (y16 == ((Two, Thr) :: q1) & _________________check q1) | (y16 == ((One, Two) :: q1) & ____________________check q1))));

_________________check y17 = (fresh q1 in (((y17 == ((One, Two) :: q1) & __________________check q1) | (y17 == ((One, Thr) :: q1) & ___________________check q1) | (y17 == ((Thr, Two) :: q1) & ________________check q1))));

__________________check y18 = (fresh q1 in (((y18 == ((Two, One) :: q1) & _________________check q1) | (y18 == ((Thr, One) :: q1) & _____________________check q1) | (y18 == ((Two, Thr) :: q1) & ___________________check q1))));

___________________check y19 = (fresh q1 in (((y19 == ((Two, One) :: q1) & _________________________check q1) | (y19 == ((Thr, One) :: q1) & _________________check q1) | (y19 == ((Thr, Two) :: q1) & __________________check q1))));

____________________check y20 = (fresh q1 in (((y20 == ((Two, One) :: q1) & ________________check q1) | (y20 == ((Two, Thr) :: q1) & _______________check q1))));

_____________________check y21 = (fresh q1 in (((y21 == ((One, Thr) :: q1) & __________________check q1) | (y21 == ((Two, Thr) :: q1) & ______________check q1) | (y21 == ((Two, One) :: q1) & _____________check q1))));

______________________check y22 = (fresh q1 in (((y22 == ((One, Two) :: q1) & ________________________check q1) | (y22 == ((Thr, Two) :: q1) & ___________check q1) | (y22 == ((Thr, One) :: q1) & ____________check q1))));

_______________________check y23 = (fresh q1 in ((y23 == [] | (y23 == ((Thr, One) :: q1) & _________check q1) | (y23 == ((Thr, Two) :: q1) & __________check q1))));

________________________check y24 = (fresh q1 in (((y24 == ((Two, One) :: q1) & ______________________check q1) | (y24 == ((Thr, One) :: q1) & ________check q1) | (y24 == ((Thr, Two) :: q1) & _______check q1))));

_________________________check y25 = (fresh q1 in (((y25 == ((One, Two) :: q1) & ___________________check q1) | (y25 == ((Thr, Two) :: q1) & __check q1) | (y25 == ((Thr, One) :: q1) & ___check q1))));

__________________________check y26 = (fresh q1 in (((y26 == ((One, Two) :: q1) & _____check q1) | (y26 == ((Thr, Two) :: q1) & _check q1) | (y26 == ((Thr, One) :: q1) & check q1))));


? check x0