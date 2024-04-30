multiply y0 y1 = (fresh q1, q2 in (((y1 == S (q1) & y0 == O & multiply O q1) | (y1 == S (q1) & y0 == S (q2) & addMultiply q1 q2))));

addMultiply y2 y4 = (fresh q1, q2 in (((y4 == O & y2 == S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (q1))))))))))))))) & _multiply q1) | (y4 == S (q2) & _addMultiply y2 q2))));

_multiply y5 = y5 == O;

_addMultiply y6 y8 = (fresh q1, q2 in (((y8 == O & y6 == S (S (S (S (S (S (S (q1))))))) & __multiply q1) | (y8 == S (q2) & __addMultiply y6 q2))));

__multiply y9 = y9 == O;

__addMultiply y10 y12 = (fresh q1 in ((y12 == S (q1) & ___addMultiply y10 q1)));

___addMultiply y13 y15 = (fresh q1, q2 in (((y15 == O & y13 == S (S (S (q1))) & ___multiply q1) | (y15 == S (q2) & ____addMultiply y13 q2))));

___multiply y16 = y16 == O;

____addMultiply y17 y19 = (fresh q1 in ((y19 == S (q1) & _____addMultiply y17 q1)));

_____addMultiply y20 y22 = (fresh q1 in ((y22 == S (q1) & ______addMultiply y20 q1)));

______addMultiply y23 y25 = (fresh q1 in ((y25 == S (q1) & _______addMultiply y23 q1)));

_______addMultiply y26 y28 = (fresh q1, q2 in (((y28 == O & y26 == S (q1) & ____multiply q1) | (y28 == S (q2) & ________addMultiply y26 q2))));

____multiply y29 = y29 == O;

________addMultiply y30 y32 = (fresh q1 in ((y32 == S (q1) & _________addMultiply y30 q1)));

_________addMultiply y33 y35 = (fresh q1 in ((y35 == S (q1) & __________addMultiply y33 q1)));

__________addMultiply y36 y38 = (fresh q1 in ((y38 == S (q1) & ___________addMultiply y36 q1)));

___________addMultiply y39 y41 = (fresh q1 in ((y41 == S (q1) & ____________addMultiply y39 q1)));

____________addMultiply y42 y44 = (fresh q1 in ((y44 == S (q1) & _____________addMultiply y42 q1)));

_____________addMultiply y45 y47 = (fresh q1 in ((y47 == S (q1) & ______________addMultiply y45 q1)));

______________addMultiply y48 y50 = (y50 == S (O) & _____multiply y48);

_____multiply y51 = y51 == O;


? multiply x0 x1