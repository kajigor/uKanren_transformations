evaloT(St, conj(X, Y)) :- evaloTT(St, X, Y).
evaloT(St, disj(X, Y)) :- evaloTT(St, X, Y).
evaloT(St, disj(X, Y)) :- evaloTF(St, X, Y).
evaloT(St, disj(X, Y)) :- evaloFT(St, X, Y).
evaloT(St, neg(X)) :- evaloF(St, X).
evaloT(St, var(Z)) :- elemoT(St, Z). 

evaloF(St, conj(X, Y)) :- evaloFF(St, X, Y).
evaloF(St, conj(X, Y)) :- evaloTF(St, X, Y).
evaloF(St, conj(X, Y)) :- evaloFT(St, X, Y).
evaloF(St, disj(X, Y)) :- evaloFF(St, X, Y).
evaloF(St, neg(X)) :- evaloT(St, X).
evaloF(St, var(Z)) :- elemoF(St, Z).

evaloTT(St, X, Y) :- evaloT(St, X), evaloT(St, Y). 
evaloTF(St, X, Y) :- evaloT(St, X), evaloF(St, Y). 
evaloFT(St, X, Y) :- evaloF(St, X), evaloT(St, Y).
evaloFF(St, X, Y) :- evaloF(St, X), evaloF(St, Y).

elemoT(cons(true, Q1), o).
elemoT(cons(Q3, Q1), s(Q2)) :- elemoT(Q1, Q2). 

elemoF(cons(false, Q1), o). 
elemoF(cons(Q3, Q1), s(Q2)) :- elemoF(Q1, Q2).


