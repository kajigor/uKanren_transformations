doubleAppendo(X, Y, Z, R) :- appendo(X, Y, T), appendo(T, Z, R).
appendo(nil, Y, Y).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).