sorto(cons(Q1, cons(Q2, cons(Q3, nil)))) :- minmaxoMinmaxo(Q1, Q2, Q3).
minmaxoMinmaxo(zero, Y3, Y4) :- minmaxo(Y3, Y4).
minmaxoMinmaxo(succ(zero), Y3, Y4) :- _minmaxo(Y3, Y4).
minmaxo(succ(zero), succ(zero)).
_minmaxo(zero, succ(zero)).
_minmaxo(succ(zero), zero).