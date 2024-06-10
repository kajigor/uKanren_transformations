fail() :- fail().
addo(zero, Y, Y).
addo(succ(X_0), Y, Z) :- addo(X_0, succ(Y), Z).
evalo(num(R), R).
evalo(sum(X, Y), R) :- evalo(X, Xr), evalo(Y, Yr), addo(Xr, Yr, R).