evalo y0 = (___evaloEvalo y0 Trueo | __evaloEvalo y0 Trueo | ___evaloEvalo y0 Falso);

andoEvaloEvaloEvaloEvalo y5 y6 y7 y8 = ((__evaloEvalo y5 y7 & ___evaloEvalo y6 y8) | (___evaloEvalo y5 y7 & __evaloEvalo y6 y8) | (__evaloEvalo y5 y7 & __evaloEvalo y6 y8));

__evaloEvalo y9 y10 = (fresh q1, q2, q3, q4 in (((y10 == Falso & y9 == Lit (Falso)) | (y9 == Neg (q1) & ___evaloEvalo q1 q2 & noto y10 q2) | (y9 == Disj (q1) (q3) & __evaloEvalo q1 q2 & __evaloEvalo q3 q4 & oro y10 q2 q4) | (y9 == Conj (q1) (q3) & andoEvaloEvaloEvaloEvalo q1 q3 q2 q4 & ando y10 q2 q4) | (y9 == Impl (q1) (q3) & ___evaloEvalo q1 q2 & __evaloEvalo q3 q4 & implicationo y10 q2 q4))));

noto y11 y12 = ((y12 == Trueo & y11 == Falso) | (y12 == Falso & y11 == Trueo));

oro y13 y14 y15 = ((y15 == Trueo & y14 == Trueo & y13 == Trueo) | (y15 == Trueo & y14 == Falso & y13 == Trueo) | (y15 == Falso & y14 == Trueo & y13 == Trueo) | (y15 == Falso & y14 == Falso & y13 == Falso));

ando y16 y17 y18 = ((y18 == Trueo & y17 == Trueo & y16 == Trueo) | (y18 == Trueo & y17 == Falso & y16 == Falso) | (y18 == Falso & y17 == Trueo & y16 == Falso) | (y18 == Falso & y17 == Falso & y16 == Falso));

implicationo y19 y20 y21 = ((y21 == Trueo & y20 == Falso & y19 == Trueo) | (y21 == Falso & y20 == Falso & y19 == Trueo) | (y21 == Trueo & y20 == Trueo & y19 == Trueo) | (y21 == Falso & y20 == Trueo & y19 == Falso));

___evaloEvalo y22 y23 = (fresh q1, q2, q3, q4 in (((y23 == Trueo & y22 == Lit (Trueo)) | (y22 == Neg (q1) & __evaloEvalo q1 q2 & noto y23 q2) | (y22 == Disj (q1) (q3) & oroEvaloEvaloEvaloEvalo q1 q3 q2 q4 & oro y23 q2 q4) | (y22 == Conj (q1) (q3) & ___evaloEvalo q1 q2 & ___evaloEvalo q3 q4 & ando y23 q2 q4) | (y22 == Impl (q1) (q3) & implicationoEvaloEvaloEvaloEvalo q1 q3 q2 q4 & implicationo y23 q2 q4))));

oroEvaloEvaloEvaloEvalo y26 y27 y28 y29 = ((___evaloEvalo y26 y28 & ___evaloEvalo y27 y29) | (__evaloEvalo y26 y28 & ___evaloEvalo y27 y29) | (___evaloEvalo y26 y28 & __evaloEvalo y27 y29));

implicationoEvaloEvaloEvaloEvalo y32 y33 y34 y35 = ((__evaloEvalo y32 y34 & ___evaloEvalo y33 y35) | (__evaloEvalo y32 y34 & __evaloEvalo y33 y35) | (___evaloEvalo y32 y34 & ___evaloEvalo y33 y35));


? evalo x0