fail() :- fail().
applasto(L, X, Lst) :- appendo(L, cons(X, nil), Lx), lasto(Lst, Lx).
lasto(X, cons(X, nil)).
lasto(X, cons(H, T)) :- lasto(X, T).
appendo(nil, Ys, Ys).
appendo(cons(H, T), Ys, cons(H, Ts)) :- appendo(T, Ys, Ts).