_evalo(conj(Q1, Q2), Y3) :- evaloEvalo(Y3, Q1, Q2).
_evalo(disj(Q1, Q2), Y3) :- evaloEvalo(Y3, Q1, Q2).
_evalo(disj(Q1, Q2), Y3) :- __evalo(Y3, Q1), _evalo(Q2, Y3).
_evalo(disj(Q1, Q2), Y3) :- ___evaloEvalo(Y3, Q1, Q2).
_evalo(neg(Q1), Y3) :- __evalo(Y3, Q1).
_evalo(var(o), cons(true, Q4)).
_evalo(var(s(Q5)), cons(Q6, Q4)) :- _elemo(Q4, Q5).
evaloEvalo(Y4, Y5, Y6) :- _evalo(Y5, Y4), _evalo(Y6, Y4).
__evalo(Y7, conj(Q1, Q2)) :- _evaloEvalo(Y7, Q1, Q2).
__evalo(Y7, conj(Q1, Q2)) :- _evalo(Q1, Y7), __evalo(Y7, Q2).
__evalo(Y7, conj(Q1, Q2)) :- __evaloEvalo(Y7, Q1, Q2).
__evalo(Y7, disj(Q1, Q2)) :- __evaloEvalo(Y7, Q1, Q2).
__evalo(Y7, neg(Q1)) :- _evalo(Q1, Y7).
__evalo(cons(false, Q4), var(o)).
__evalo(cons(Q6, Q4), var(s(Q5))) :- elemo(Q4, Q5).
_evaloEvalo(Y9, Y10, Y11) :- __evalo(Y9, Y10), _evalo(Y11, Y9).
__evaloEvalo(Y12, Y13, Y14) :- __evalo(Y12, Y13), __evalo(Y12, Y14).
elemo(cons(false, Q1), o).
elemo(cons(Q3, Q1), s(Q2)) :- elemo(Q1, Q2).
___evaloEvalo(Y17, Y18, Y19) :- _evalo(Y18, Y17), __evalo(Y17, Y19).
_elemo(cons(true, Q1), o).
_elemo(cons(Q3, Q1), s(Q2)) :- _elemo(Q1, Q2).