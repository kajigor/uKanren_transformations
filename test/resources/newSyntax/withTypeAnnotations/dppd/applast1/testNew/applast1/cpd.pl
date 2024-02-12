applasto(cons(s(o), Q1)) :- _appendo(Q1, nil).
applasto(cons(Q2, Q1)) :- appendoLasto(Q1).
appendoLasto(cons(Q1, Q2)) :- _appendo(Q2, Q3), lasto(cons(Q1, Q3)).
_appendo(nil, cons(o, nil)).
_appendo(cons(Q1, Q3), cons(Q1, Q2)) :- _appendo(Q3, Q2).
lasto(cons(s(o), nil)).
lasto(cons(Q1, Q2)) :- lasto(Q2).