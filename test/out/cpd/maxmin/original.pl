fail() :- fail().
le(zero, Y, trueo).
le(succ(Z), zero, falso).
le(succ(X_0), succ(Y_0), B) :- le(X_0, Y_0, B).
gt(zero, Y, falso).
gt(succ(Z), zero, trueo).
gt(succ(X_0), succ(Y_0), B) :- gt(X_0, Y_0, B).
maxmin(nil, zero, zero).
maxmin(cons(H, T), A, I) :- max(T, H, A), min(T, H, I).
max(nil, N, N).
max(cons(H, T), N, M) :- le(H, N, trueo), max(T, N, M).
max(cons(H, T), N, M) :- gt(H, N, trueo), max(T, H, M).
min(nil, N, N).
min(cons(H, T), N, M) :- le(H, N, trueo), min(T, H, M).
min(cons(H, T), N, M) :- gt(H, N, trueo), min(T, N, M).