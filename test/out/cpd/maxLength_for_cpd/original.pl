fail() :- fail().
le(zero, Y).
le(succ(T), succ(T1)) :- le(T, T1).
max_length(Ls, M, Len) :- max(Ls, M), my_length(Ls, Len).
my_length(nil, zero).
my_length(cons(H, T), succ(Lent)) :- my_length(T, Lent).
max(X, M) :- max1(X, zero, M).
max1(nil, M, M).
max1(cons(X, T), N, M) :- le(X, N), max1(T, N, M).
max1(cons(X, T), N, M) :- le(N, X), max1(T, X, M).