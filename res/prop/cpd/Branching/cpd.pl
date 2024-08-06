evalo(lit(trueo)).
evalo(disj(Q1, Q2)) :- evalo(Q1), evalo(Q2).
evalo(disj(Q1, Q2)) :- _evalo(Q1), evalo(Q2).
evalo(disj(Q1, Q2)) :- evalo(Q1), _evalo(Q2).
evalo(conj(Q1, Q2)) :- evalo(Q1), evalo(Q2).
_evalo(lit(falso)).
_evalo(disj(Q1, Q2)) :- _evalo(Q1), _evalo(Q2).
_evalo(conj(Q1, Q2)) :- _evalo(Q1), evalo(Q2).
_evalo(conj(Q1, Q2)) :- evalo(Q1), _evalo(Q2).
_evalo(conj(Q1, Q2)) :- _evalo(Q1), _evalo(Q2).