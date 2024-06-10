fail() :- fail().
add(o, Z, Z).
add(s(T1), Y, s(T2)) :- add(T1, Y, T2).
multiply(X, o, o).
multiply(X, s(T), Z) :- add(X, Z1, Z), multiply(X, T, Z1).
sumsquaresupto(N, S) :- upto(s(o), N, Ns), squares(Ns, Sons), sum(Sons, S).
le(o, N).
le(s(T), s(T1)) :- le(T, T1).
upto(s(N), N, nil).
upto(M, N, cons(M, T)) :- le(M, N), upto(s(M), N, T).
square(Ns, Son) :- multiply(Ns, Ns, Son).
squares(nil, nil).
squares(cons(H1, T1), cons(H2, T2)) :- square(H1, H2), squares(T1, T2).
sum(Ns, S) :- sum1(Ns, o, S).
sum1(nil, S2, S2).
sum1(cons(H, T), S1, S2) :- add(H, S1, S0), sum1(T, S0, S2).