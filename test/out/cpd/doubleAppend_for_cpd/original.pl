fail() :- fail().
appendo(nil, Xy, Xy).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).
double_appendo(X, Y, Z, Res) :- appendo(X, Y, T), appendo(T, Z, Res).