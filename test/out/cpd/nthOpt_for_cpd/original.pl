fail() :- fail().
nthOpt(nil, N, none).
nthOpt(cons(H, T), zero, some(H)).
nthOpt(cons(H, T), succ(X), R) :- nthOpt(T, X, R).