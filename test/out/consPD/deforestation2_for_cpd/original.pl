fail() :- fail().
neq(o, s(T)).
neq(s(T), o).
neq(s(Tx), s(Ty)) :- neq(Tx, Ty).
rr(X, Y) :- r(T, Y), r(X, T).
r(nil, nil).
r(cons(A, cons(A, T)), cons(A, T1)) :- r(T, T1).
r(cons(Ax, cons(Ay, T)), cons(Ax, T1)) :- neq(Ax, Ay), r(cons(Ay, T), T1).