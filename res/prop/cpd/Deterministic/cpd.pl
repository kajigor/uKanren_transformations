evalo(lit(trueo)).
evalo(disj(Q1, Q2)) :- oroEvaloEvalo(Q1, Q2).
evalo(conj(Q1, Q2)) :- evalo(Q1), evalo(Q2).
oroEvaloEvalo(Y1, Y2) :- evalo(Y1), evalo(Y2).
oroEvaloEvalo(Y1, Y2) :- _evalo(Y1), evalo(Y2).
oroEvaloEvalo(Y1, Y2) :- evalo(Y1), _evalo(Y2).
_evalo(lit(falso)).
_evalo(disj(Q1, Q2)) :- _evalo(Q1), _evalo(Q2).
_evalo(conj(Q1, Q2)) :- andoEvaloEvalo(Q1, Q2).
andoEvaloEvalo(Y6, Y7) :- _evalo(Y6), evalo(Y7).
andoEvaloEvalo(Y6, Y7) :- evalo(Y6), _evalo(Y7).
andoEvaloEvalo(Y6, Y7) :- _evalo(Y6), _evalo(Y7).