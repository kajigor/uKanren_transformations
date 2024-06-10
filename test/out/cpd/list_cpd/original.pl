fail() :- fail().
appendo(nil, Xy, Xy).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).
reverso(nil, nil).
reverso(cons(H, T), Y) :- reverso(T, Rt), appendo(Rt, cons(H, nil), Y).
help(Xs, Ys, Ts) :- appendo(Xs, Ys, Ts), reverso(Ts, Ys).