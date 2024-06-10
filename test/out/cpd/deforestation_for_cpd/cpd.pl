rr(nil, nil).
rr(cons(Q1, cons(Q1, Q2)), Y1) :- rR(Y1, Q1, Q2).
rr(cons(Q3, cons(Q4, Q5)), Y1) :- neqRR(Y1, Q3, Q4, Q5).
rR(Y2, Y3, cons(Q1, cons(Q1, Q2))) :- r(Q2, Q3), r(cons(Y3, cons(Q1, Q3)), Y2).
rR(Y2, Y3, cons(Q4, cons(Q5, Q6))) :- neq(Q4, Q5), r(cons(Q5, Q6), Q7), r(cons(Y3, cons(Q4, Q7)), Y2).
r(nil, nil).
r(cons(Q1, cons(Q1, Q3)), cons(Q1, Q2)) :- r(Q3, Q2).
r(cons(Q4, cons(Q6, Q7)), cons(Q4, Q5)) :- neq(Q4, Q6), r(cons(Q6, Q7), Q5).
neq(o, s(Q1)).
neq(s(Q2), o).
neq(s(Q4), s(Q3)) :- neq(Q4, Q3).
neqRR(Y10, o, s(Q1), Y13) :- rR(Y10, o, cons(s(Q1), Y13)).
neqRR(Y10, s(Q2), o, Y13) :- rR(Y10, s(Q2), cons(o, Y13)).
neqRR(Y10, s(Q4), s(Q3), Y13) :- rR(Y10, s(Q4), cons(s(Q3), Y13)), neq(Q4, Q3).