reverso(nil, nil).
reverso(cons(H, T), Y) :- reverso(T, Rt), appendo(Rt, cons(H, nil), Y).
appendo(nil, Y, Y).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).