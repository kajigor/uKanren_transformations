evalo(zero, succ(succ(zero))).
evalo(succ(Q1), Y1) :- addoAddo(Y1, Q1).
addoAddo(succ(zero), zero).
addoAddo(Y3, succ(Q1)) :- addoAddo(succ(Y3), Q1).