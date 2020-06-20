appendo(nil, Y2, Y2).
appendo(cons(Q1, nil), Q3, cons(Q1, Q3)).
appendo(cons(Q1, cons(Q4, nil)), Q6, cons(Q1, cons(Q4, Q6))).
appendo(cons(Q1, cons(Q4, cons(Q7, Q8))), Y1, cons(Q1, cons(Q4, cons(Q7, Q9)))) :- _appendo(Y1, Q8, Q9).
_appendo(Y5, nil, Y5).
_appendo(Y3, cons(Q1, Q2), cons(Q1, Q3)) :- _appendo(Y3, Q2, Q3).