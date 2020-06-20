sorto(nil, nil).
sorto(X, cons(S, Xs')) :- sorto(Xs, Xs'), smallesto(X, S, Xs).
smallesto(cons(S, nil), S, nil).
smallesto(cons(H, T), S, cons(Max, T')) :- minmaxo(H, S', S, Max), smallesto(T, S', T').
minmaxo(A, B, A, B) :- leo(A, B, true).
minmaxo(A, B, B, A) :- gto(A, B, true).
leo(o, Y, true).
leo(s(Zz), o, false).
leo(s(X'), s(Y'), B) :- leo(X', Y', B).
gto(s(Zz), o, true).
gto(o, Y, false).
gto(s(X'), s(Y'), B) :- gto(X', Y', B).