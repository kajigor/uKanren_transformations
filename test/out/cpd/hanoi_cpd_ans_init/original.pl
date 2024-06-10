fail() :- fail().
check(cons(Q1, Q2)) :- one_stepCheck(Q1, Q2).
one_stepCheck(pair(one, Q1), Y2) :- notEqStickGetSetCheck(Y2, Q1).
notEqStickGetSetCheck(Y4, two) :- _check(Y4).
notEqStickGetSetCheck(Y4, thr) :- ___check(Y4).
_check(cons(Q1, Q2)) :- _one_stepCheck(Q1, Q2).
_one_stepCheck(pair(Q1, thr), Y9) :- _notEqStickGetSetCheck(Y9, Q1).
_notEqStickGetSetCheck(Y11, two) :- ___check(Y11).
___check(cons(Q1, Q2)) :- ___one_stepCheck(Q1, Q2).
___one_stepCheck(pair(Q1, two), Y24) :- __notEqStickGetSetCheck(Y24, Q1).
__notEqStickGetSetCheck(Y26, thr) :- _check(Y26).