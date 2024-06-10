rr(Y0) :- r(Q1), _r(Y0, Q1).
r(cons(s(o), cons(s(o), cons(o, cons(o, cons(s(s, o), cons(s(s, o), cons(o, cons(o, cons(o, cons(o, nil))))))))))).
r(cons(s(o), cons(o, cons(o, Q1)))) :- __r(Q1).
_r(nil, nil).
_r(cons(Q1, cons(Q1, Q3)), cons(Q1, Q2)) :- _r(Q3, Q2).
_r(cons(Q4, cons(Q6, Q7)), cons(Q4, Q5)) :- neq(Q4, Q6), _r(cons(Q6, Q7), Q5).
__r(cons(s(s, o), cons(s(s, o), cons(o, cons(o, cons(o, cons(o, nil))))))).
neq(o, s(Q1)).
neq(s(Q2), o).
neq(s(Q4), s(Q3)) :- neq(Q4, Q3).