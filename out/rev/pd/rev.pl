_reverso(nil, nil).
_reverso(cons(Q1, Q2), cons(Q1, nil)) :- _reverso(Q2, nil).
_reverso(cons(Q1, Q2), cons(Q4, Q6)) :- _reverso(Q2, cons(Q4, Q5)), appendo(Q6, Q1, Q5).
appendo(cons(Y5, nil), Y5, nil).
appendo(cons(Q1, Q3), Y5, cons(Q1, Q2)) :- appendo(Q3, Y5, Q2).