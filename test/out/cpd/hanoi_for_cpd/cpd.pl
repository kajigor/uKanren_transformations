check(cons(Q1, Q2)) :- one_stepCheck(Q1, Q2).
one_stepCheck(pair(one, Q1), Y2) :- notEqStickGetSetCheck(Y2, Q1).
notEqStickGetSetCheck(Y4, two) :- _check(Y4).
notEqStickGetSetCheck(Y4, thr) :- ___check(Y4).
_check(cons(Q1, Q2)) :- _one_stepCheck(Q1, Q2).
_one_stepCheck(pair(Q1, thr), Y9) :- _notEqStickGetSetCheck(Y9, Q1).
_one_stepCheck(pair(Q1, Q2), Y9) :- _notEqStickGetGetLessSetSetCheck(Y9, Q3, Q1, Q2, Q4, Q5, Q6, Q7, Q8).
_notEqStickGetSetCheck(Y11, one) :- __check(Y11).
_notEqStickGetSetCheck(Y11, two) :- ___check(Y11).
__check(cons(Q1, Q2)) :- __one_stepCheck(Q1, Q2).
__one_stepCheck(pair(Q1, Q2), Y20) :- notEqStickGetGetLessSetSetCheck(Y20, Q3, Q1, Q2, Q4, Q5, Q6, Q7, Q8).
___check(cons(Q1, Q2)) :- ___one_stepCheck(Q1, Q2).
___one_stepCheck(pair(Q1, two), Y24) :- __notEqStickGetSetCheck(Y24, Q1).
___one_stepCheck(pair(Q1, Q2), Y24) :- ___notEqStickGetGetLessSetSetCheck(Y24, Q3, Q1, Q2, Q4, Q5, Q6, Q7, Q8).
__notEqStickGetSetCheck(Y26, one) :- ____check(Y26).
__notEqStickGetSetCheck(Y26, thr) :- _check(Y26).
____check(cons(Q1, Q2)) :- ____one_stepCheck(Q1, Q2).
____one_stepCheck(pair(Q1, Q2), Y35) :- __notEqStickGetGetLessSetSetCheck(Y35, Q3, Q1, Q2, Q4, Q5, Q6, Q7, Q8).