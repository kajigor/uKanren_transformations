fail() :- fail().
depth(c(true, nil), o).
depth(c(cons, L, R), D) :- depth(L, D1), depth(R, D2), max(D1, D2, D).
depth(Exp, s(D1)) :- prog_clause(Exp, Body), depth(Body, D1).
max(Mx, o, Mx).
max(o, Mx, Mx).
max(s(A1), s(B1), s(Mx1)) :- max(A1, B1, Mx1).
prog_clause(c(member, X, Xs), c(append, Y, cons(X, Z), Xs)).
prog_clause(c(append, nil, X, X), c(true, nil)).
prog_clause(c(append, cons(X, L1), L2, cons(X, L3)), c(append, L1, L2, L3)).