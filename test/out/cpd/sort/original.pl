fail() :- fail().
sorto(nil, nil).
sorto(cons(H, T), Reslst) :- splito(H, T, L, G), sorto(L, Lres), sorto(R, Rres), appendo(Lres, cons(H, Rres), Reslst).
splito(X, nil, nil, nil).
splito(X, cons(X1, Xs1), cons(X1, L1), G) :- le(X1, X), splito(X, Xs1, L1, G).
splito(X, cons(X1, Xs1), L, cons(X1, G1)) :- gt(X1, X), splito(X, Xs1, L, G1).
appendo(nil, Xy, Xy).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).
gt(succ(Z), zero).
gt(succ(X_0), succ(Y_0)) :- gt(X_0, Y_0).
le(zero, Y).
le(succ(X_0), succ(Y_0)) :- le(X_0, Y_0).
fail() :- fail(nil).