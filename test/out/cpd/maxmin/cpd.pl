maxmin(nil, zero, zero).
maxmin(cons(Q1, Q2), Y1, Y2) :- _maxMin(Y1, Y2, Q1, Q2, Q2).
_maxMin(Y9, Y8, Y9, nil, Y11) :- min(Y8, Y9, Y11).
_maxMin(Y7, Y8, Y9, cons(Q1, Q2), Y11) :- _maxMin(Y7, Y8, Y9, Q2, Y11), le(Y9, Q1).
_maxMin(Y7, Y8, Y9, cons(Q1, Q2), Y11) :- gt(Y9, Q1), max(Y7, Q2, Q1), min(Y8, Y9, Y11).
le(Y12, zero).
le(succ(Q2), succ(Q1)) :- le(Q2, Q1).
max(Y16, nil, Y16).
max(Y14, cons(Q1, Q2), Y16) :- le(Y16, Q1), max(Y14, Q2, Y16).
max(Y14, cons(Q1, Q2), Y16) :- gt(Y16, Q1), max(Y14, Q2, Q1).
min(Y18, Y18, nil).
min(Y17, Y18, cons(Q1, Q2)) :- le(Y18, Q1), min(Y17, Q1, Q2).
min(Y17, Y18, cons(Q1, Q2)) :- gt(Y18, Q1), min(Y17, Y18, Q2).
gt(zero, succ(Q1)).
gt(succ(Q3), succ(Q2)) :- gt(Q3, Q2).