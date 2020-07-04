eqNat(z, z, true).
eqNat(s(Q27), z, false).
eqNat(z, s(Q29), false).
eqNat(s(X), s(Y), Q24) :- eqNat(X, Y, Q24).
eqPair(pair(A1, A2), pair(B1, B2), false) :- eqNat(A1, B1, false), eqNat(A2, B2, Q18).
eqPair(pair(A1, A2), pair(B1, B2), Q18) :- eqNat(A1, B1, true), eqNat(A2, B2, Q18).
elem(X, nil, false).
elem(X, cons(Y, Ys), true) :- eqPair(X, Y, true).
elem(X, cons(Y, Ys), Q10) :- eqPair(X, Y, false), elem(X, Ys, Q10).
isPath(nil, G, true).
isPath(cons(Q2, nil), G, true).
isPath(cons(X1, cons(X2, Xs)), G, false) :- elem(pair(X1, X2), G, false), isPath(cons(X2, Xs), G, Q5).
isPath(cons(X1, cons(X2, Xs)), G, Q5) :- elem(pair(X1, X2), G, true), isPath(cons(X2, Xs), G, Q5).