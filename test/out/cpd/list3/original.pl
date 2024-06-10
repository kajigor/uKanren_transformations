fail() :- fail().
reverso(nil, nil).
reverso(cons(H, T), Y) :- reverso(T, Rt), appendo(Rt, cons(H, nil), Y).
appendo(nil, Xy, Xy).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).
help(Xs, Ys, Zs, Ts) :- appendo(Xs, Ys, Ts), reverso(Zs, Ts).