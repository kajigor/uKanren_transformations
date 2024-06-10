fail() :- fail().
listo(nil).
listo(cons(H, T)) :- listo(T).
membero(H, cons(H, T)).
membero(X, cons(H, T)) :- membero(X, T).
inBotho(X, Ys, Zs) :- membero(X, Ys), membero(X, Zs).
nilo(nil).
singletono(cons(X, nil), X).
maxLengtho(X, M, L) :- maxo(X, M), lengtho(X, L).
maxMino(X, M, L) :- maxo(X, M), mino(X, L).
copy(nil, nil).
copy(cons(H, T), cons(H, T_0)) :- copy(T, T_0).
copy2(nil, nil).
copy2(cons(H, nil), cons(H, nil)).
copy2(cons(H1, cons(H2, T)), cons(H1, T_0)) :- copy2(T, T_0).
copycopy(L, L1, L2) :- copy(L, L1), copy2(L, L2).
lengtho(nil, zero).
lengtho(cons(H, T), succ(Z)) :- lengtho(T, Z).
lengtho_0(nil, zero).
lengtho_0(cons(H, T), succ(Z)) :- lengtho_0(T, Z).
maxo(X, M) :- maxo1(X, zero, M).
maxo1(nil, N, N).
maxo1(cons(H, T), N, M) :- leo(H, N, trueo), maxo1(T, N, M).
maxo1(cons(H, T), N, M) :- gto(H, N, trueo), maxo1(T, H, M).
mino(nil, zero).
mino(cons(H, T), M) :- mino1(T, H, M).
mino1(nil, N, N).
mino1(cons(H, T), N, M) :- leo(H, N, trueo), mino1(T, H, M).
mino1(cons(H, T), N, M) :- gto(H, N, trueo), mino1(T, N, M).
appLengtho() :- appendo(Xs, Ys, Zs), lengtho(Xs, M), lengtho(Ys, N), lengtho(Zs, S), addo(M, N, S).
appendo(nil, Xy, Xy).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).
appendo_0(nil, Y, Y).
appendo_0(cons(H, T), Y, Xy).
appendo_0(X, Y, cons(H, Ty)).
appendo_0(X, Y, Xy) :- appendo_0(T, Y, Ty).
reverso(nil, nil).
reverso(cons(H, T), Y) :- reverso(T, Rt), appendo(Rt, cons(H, nil), Y).
doubleReverso(Xs) :- reverso(Xs, Sx), reverso(Sx, Xs).
revAcco(nil, Acc, Acc).
revAcco(cons(H, T), Acc, Sx) :- revacco(T, cons(H, Acc), Sx).
assoco(X, cons(pair(X, V), Tl), V).
assoco(X, cons(pair(A, B), Tl), V) :- assoco(X, Tl, V).
nthOpt(nil, N, none).
nthOpt(cons(H, T), zero, some(H)).
nthOpt(cons(H, T), succ(X), R) :- nthOpt(T, succ(X), R).
fail() :- fail().
notZero(s(Y)).
addo(o, Z, Z).
addo(s(X_0), Y, s(Z_0)) :- addo(X_0, Y, Z_0).
mulo(o, Y, o).
mulo(s(X_0), Y, Z) :- addo(Y, Z_0, Z), mulo(X_0, Y, Z_0).
leo(o, Y, trueo).
leo(s(Zz), o, falso).
leo(s(X_0), s(Y_0), B) :- leo(X_0, Y_0, B).
gto(s(Zz), o, trueo).
gto(o, Y, falso).
gto(s(X_0), s(Y_0), B) :- gto(X_0, Y_0, B).
geo(X, Y, Z) :- leo(Y, X, Z).
lto(X, Y, Z) :- gto(Y, X, Z).