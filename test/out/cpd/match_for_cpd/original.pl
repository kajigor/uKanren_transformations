fail() :- fail().
matcho(P, S) :- appendo(S1, T1, S), appendo(T2, P, S1).
appendo(nil, Xy, Xy).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).