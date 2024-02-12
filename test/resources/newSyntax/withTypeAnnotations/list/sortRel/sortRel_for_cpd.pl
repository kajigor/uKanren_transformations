fail() :- fail().
leo(zero, Y, trueo).
leo(succ(Z), zero, falso).
leo(succ(X_0), succ(Y_0), B) :- leo(X_0, Y_0, B).
gto(zero, Y, falso).
gto(succ(Z), zero, trueo).
gto(succ(X_0), succ(Y_0), B) :- gto(X_0, Y_0, B).
minmaxo(A, Max, A, Max) :- leo(A, Max, trueo).
minmaxo(A, Min, Min, A) :- gto(A, Min, trueo).
smallesto(cons(S, nil), S, nil).
smallesto(cons(H, T), S, cons(Max, T_0)) :- minmaxo(H, S_0, S, Max), smallesto(T, S_0, T_0).
sorto(nil, nil).
sorto(X, cons(S, Xs_0)) :- sorto(Xs, Xs_0), smallesto(X, S, Xs).