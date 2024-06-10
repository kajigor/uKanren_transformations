appendo(Y7, nil, Y7).
appendo(Y5, cons(Q1, Q3), cons(Q1, Q2)) :- appendo(Y5, Q3, Q2).
_appendoAppendo(nil, nil, nil, Y12, Y12).
_appendoAppendo(nil, cons(Q1, nil), cons(Q1, nil), Q2, cons(Q1, Q2)).
_appendoAppendo(nil, cons(Q1, cons(Q4, Q6)), cons(Q1, cons(Q4, Q6)), Y11, cons(Q1, cons(Q4, Q5))) :- appendo(Y11, Q6, Q5).
_appendoAppendo(cons(Q7, Q10), Y9, cons(Q7, Q9), Y11, cons(Q7, Q8)) :- _appendoAppendo(Q10, Y9, Q9, Y11, Q8).