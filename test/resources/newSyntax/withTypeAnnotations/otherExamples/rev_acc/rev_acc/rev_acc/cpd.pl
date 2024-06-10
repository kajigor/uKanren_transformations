rev(cons(Q1, Q2)) :- _rev(Q2, cons(Q1, nil)).
_rev(nil, cons(a, cons(b, cons(c, cons(d, nil))))).
_rev(cons(Q1, Q2), Y2) :- _rev(Q2, cons(Q1, Y2)).