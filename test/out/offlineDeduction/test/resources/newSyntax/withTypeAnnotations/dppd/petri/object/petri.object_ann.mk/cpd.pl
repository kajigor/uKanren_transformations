unsafe(Y0) :- __unsafe(Q1, s(o), o).
__unsafe(Y3, Y4, Y5) :- __unsafe(Q1, s(Y4), Y5).
__unsafe(Y3, s(Q2), Y5) :- ___unsafe(Y5, Q2).
__unsafe(Y3, s(Q3), Y5) :- __unsafe(s(Q4), Q3, s(Y5)).
___unsafe(Y6, Y8) :- __unsafe(Q1, s(Y8), s(Y6)).
___unsafe(Y6, s(Q2)) :- ___unsafe(s(Y6), Q2).
_____unsafe(Y11, Y13) :- __unsafe(Q1, s(Q2), s(s(s(s(s(s(s(s(s(Y13)))))))))).
_____unsafe(Y11, Y13) :- _____unsafe(s(Q1), s(Y13)).