fail() :- fail().
leo(zero, Y, trueo).
leo(succ(Z), zero, falso).
leo(succ(X_0), succ(Y_0), B) :- leo(X_0, Y_0, B).
gto(zero, Y, falso).
gto(succ(Z), zero, trueo).
gto(succ(X_0), succ(Y_0), B) :- gto(X_0, Y_0, B).
maxo(X, M) :- maxo1(X, zero, M).
maxo1(nil, N, N).
maxo1(cons(H, T), N, M) :- leo(H, N, trueo), maxo1(T, N, M).
maxo1(cons(H, T), N, M) :- gto(H, N, trueo), maxo1(T, H, M).
mino(nil, zero).
mino(cons(H, T), M) :- mino1(T, H, M).
mino1(nil, N, N).
mino1(cons(H, T), N, M) :- leo(H, N, trueo), mino1(T, H, M).
mino1(cons(H, T), N, M) :- gto(H, N, trueo), mino1(T, N, M).
maxMino(X, M, L) :- maxo(X, M), mino(X, L).