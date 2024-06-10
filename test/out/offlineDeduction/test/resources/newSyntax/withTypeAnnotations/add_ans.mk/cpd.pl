addo(zero, succ(succ(zero))).
addo(succ(Q1), Y1) :- _addo(Y1, Q1, succ(zero)).
_addo(succ(succ(Y4)), zero, Y4).
_addo(Y2, succ(Q1), Y4) :- _addo(Y2, Q1, succ(Y4)).