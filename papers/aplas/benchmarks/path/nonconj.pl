elem(cons(pair(Q1, Q2), Q3), Y3, Y4) :- eqNat(Y3, Q1), eqNat(Y4, Q2).
elem(cons(Q4, Q3), Y3, Y4) :- eqPair(Y3, Y4, Q4), elem(Q3, Y3, Y4).
eqNat(z, z).
eqNat(s(Q1), s(Q2)) :- eqNat(Q1, Q2).
eqPair(Y7, Y8, pair(Q1, Q2)) :- _eqNat(Y7, Q1), __eqNat(Y8, Q2).
eqPair(Y7, Y8, pair(Q1, Q2)) :- eqNat(Y7, Q1), _eqNat(Y8, Q2).
_eqNat(s(Q1), z).
_eqNat(z, s(Q2)).
_eqNat(s(Q3), s(Q4)) :- _eqNat(Q3, Q4).
__eqNat(z, z).
__eqNat(s(Q1), z).
__eqNat(z, s(Q2)).
__eqNat(s(Q3), s(Q4)) :- __eqNat(Q3, Q4).
_isPath(nil, Y16).
_isPath(cons(Q1, nil), Y16).
_isPath(cons(Q2, cons(Q3, Q4)), Y16) :- elem(Y16, Q2, Q3), _isPath(cons(Q3, Q4), Y16).