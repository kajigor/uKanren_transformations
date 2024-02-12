unsafe(Y0) :- ____unsafe(Q1, s(o), o).
___unsafe(Y5, Y6, Y7) :- ____unsafe(Q1, s(Y6), Y7).
___unsafe(Y5, s(Q2), Y7) :- ___unsafe(s(Q1), Q2, s(Y7)).
____unsafe(Y8, Y9, Y10) :- ___unsafe(Q1, Y9, Y10).
____unsafe(Y8, s(Q2), Y10) :- ____unsafe(s(Q3), Q2, s(Y10)).