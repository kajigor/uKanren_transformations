evaloT(St, conj(X, Y), s(N)) :- evaloT(St, X, N), evaloT(St, Y, N).
evaloT(St, disj(X, Y), s(N)) :- evaloT(St, X, N), evaloT(St, Y, N).
evaloT(St, disj(X, Y), s(N)) :- evaloT(St, X, N), evaloF(St, Y, N).
evaloT(St, disj(X, Y), s(N)) :- evaloF(St, X, N), evaloT(St, Y, N).
evaloT(St, neg(X), s(N)) :- evaloF(St, X, N).
evaloT(St, var(Z), o) :- elemoT(St, Z).

evaloF(St, conj(X, Y), s(N)) :- evaloF(St, X, N), evaloF(St, Y, N).
evaloF(St, conj(X, Y), s(N)) :- evaloT(St, X, N), evaloF(St, Y, N).
evaloF(St, conj(X, Y), s(N)) :- evaloF(St, X, N), evaloT(St, Y, N).
evaloF(St, disj(X, Y), s(N)) :- evaloF(St, X, N), evaloF(St, Y, N).
evaloF(St, neg(X), s(N)) :- evaloT(St, X, N).
evaloF(St, var(Z), o) :- elemoF(St, Z).

elemoT(cons(true, Q1), o).
elemoT(cons(Q3, Q1), s(Q2)) :- elemoT(Q1, Q2).

elemoF(cons(false, Q1), o).
elemoF(cons(Q3, Q1), s(Q2)) :- elemoF(Q1, Q2).


