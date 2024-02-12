nthOpt(nil, none).
nthOpt(cons(Q1, Q2), Y1) :- _nthOpt(Y1, Q2).
_nthOpt(none, nil).
_nthOpt(Y2, cons(Q1, Q2)) :- __nthOpt(Y2, Q2).
__nthOpt(none, nil).
__nthOpt(Y4, cons(Q1, Q2)) :- ___nthOpt(Y4, Q2).
___nthOpt(none, nil).
___nthOpt(some(Q1), cons(Q1, Q2)).