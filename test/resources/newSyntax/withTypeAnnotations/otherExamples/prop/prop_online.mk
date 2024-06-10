evalo y0 = (fresh q1, q2, q3 in ((y0 == Lit (Trueo) | (y0 == Var (q1) & elemo q1) | (y0 == Neg (q2) & _evalo q2) | (y0 == Disj (q2) (q3) & oroEvaloEvalo q2 q3) | (y0 == Conj (q2) (q3) & evalo q2 & evalo q3) | (y0 == Impl (q2) (q3) & implicationoEvaloEvalo q2 q3))));

elemo y1 = (fresh q1 in ((y1 == Zero | (y1 == Succ (q1) & _elemo q1))));

_elemo y2 = (fresh q1 in ((y2 == Zero | (y2 == Succ (q1) & __elemo q1))));

__elemo y3 = y3 == Zero;

_evalo y4 = (fresh q1, q2 in ((y4 == Lit (Falso) | (y4 == Neg (q1) & evalo q1) | (y4 == Disj (q1) (q2) & _evalo q1 & _evalo q2) | (y4 == Conj (q1) (q2) & andoEvaloEvalo q1 q2) | (y4 == Impl (q1) (q2) & evalo q1 & _evalo q2))));

andoEvaloEvalo y5 y6 = ((_evalo y5 & evalo y6) | (evalo y5 & _evalo y6) | (_evalo y5 & _evalo y6));

oroEvaloEvalo y9 y10 = ((evalo y9 & evalo y10) | (_evalo y9 & evalo y10) | (evalo y9 & _evalo y10));

implicationoEvaloEvalo y13 y14 = ((_evalo y13 & evalo y14) | (_evalo y13 & _evalo y14) | (evalo y13 & evalo y14));


? evalo x0