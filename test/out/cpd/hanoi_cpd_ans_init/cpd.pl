check(cons(pair(one, two), Q1)) :- _check(Q1).
check(cons(pair(one, thr), Q1)) :- ___check(Q1).
_check(cons(pair(two, thr), Q1)) :- ___check(Q1).
___check(cons(pair(thr, two), Q1)) :- _check(Q1).