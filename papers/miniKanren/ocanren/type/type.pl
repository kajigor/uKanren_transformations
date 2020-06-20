elemo(cons(arrow(Y5, Y6), Q1), Y5, Y6, o).
elemo(cons(Q3, Q1), Y5, Y6, s(Q2)) :- elemo(Q1, Y5, Y6, Q2).
_type(var(o), cons(arrow(Y10, Y11), Q2), Y10, Y11).
_type(var(s(Q3)), cons(Q4, Q2), Y10, Y11) :- elemo(Q2, Y10, Y11, Q3).
_type(abs(Q1, Q5, Q6), Y9, Y10, Y11) :- _type(Q6, cons(pair(Q1, Q5), Y9), Y10, Y11).
_type(app(Q7, Q8), Y9, Y10, Y11) :- _type(Q7, Y9, Q9, Q10), __type(Y9, Q8, Q10).
__type(Y12, bConst(Q1), boolean).
__type(Y12, iConst(Q2), integer).
__type(cons(Y14, Q4), var(o), Y14).
__type(cons(Q6, Q4), var(s(Q5)), Y14) :- _elemo(Q4, Y14, Q5).
__type(Y12, abs(Q3, Q7, Q8), Y14) :- __type(cons(pair(Q3, Q7), Y12), Q8, Y14).
__type(Y12, app(Q9, Q10), Y14) :- typeType(Y12, Q9, Q10).
_elemo(cons(Y16, Q1), Y16, o).
_elemo(cons(Q3, Q1), Y16, s(Q2)) :- _elemo(Q1, Y16, Q2).
typeType(Y18, Y19, Y20) :- _type(Y19, Y18, Q1, Q2), __type(Y18, Y20, Q2).