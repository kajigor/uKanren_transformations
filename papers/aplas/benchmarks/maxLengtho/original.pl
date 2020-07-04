maxLengtho(X, M, L) :- maxo(X, M), lengtho(X, L).
maxo(X, M) :- maxo1(X, o, M).
maxo1(nil, N, N).
maxo1(cons(H, T), N, M) :- leo(H, N, true), maxo1(T, N, M).
maxo1(cons(H, T), N, M) :- gto(H, N, true), maxo1(T, H, M).
leo(o, Y, true).
leo(s(Zz), o, false).
leo(s(X1), s(Y1), B) :- leo(X1, Y1, B).
gto(s(Zz), o, true).
gto(o, Y, false).
gto(s(X1), s(Y1), B) :- gto(X1, Y1, B).
lengtho(nil, o).
lengtho(cons(H, T), s(Z)) :- lengtho(T, Z).