_evalo(conj(Q1, Q2), Y3) :- ___evaloEvalo(Y3, Q1, Q2).
_evalo(disj(Q1, Q2), Y3) :- __evalo(Y3, Q1), _evalo(Q2, Y3).
_evalo(disj(Q1, Q2), Y3) :- __evaloEvalo(Y3, Q1, Q2).
_evalo(disj(Q1, Q2), Y3) :- ___evaloEvalo(Y3, Q1, Q2).
_evalo(neg(Q1), Y3) :- __evalo(Y3, Q1).
_evalo(var(o), cons(true, Q4)).
_evalo(var(s(Q5)), cons(Q6, Q4)) :- _elemo(Q4, Q5).
__evalo(Y4, conj(Q1, Q2)) :- _evaloEvalo(Y4, Q1, Q2).
__evalo(Y4, conj(Q1, Q2)) :- evaloEvalo(Y4, Q1, Q2).
__evalo(Y4, conj(Q1, Q2)) :- _evalo(Q1, Y4), __evalo(Y4, Q2).
__evalo(Y4, disj(Q1, Q2)) :- _evaloEvalo(Y4, Q1, Q2).
__evalo(Y4, neg(Q1)) :- _evalo(Q1, Y4).
__evalo(cons(false, Q4), var(o)).
__evalo(cons(Q6, Q4), var(s(Q5))) :- elemo(Q4, Q5).
evaloEvalo(Y6, Y7, Y8) :- __evalo(Y6, Y7), _evalo(Y8, Y6).
_evaloEvalo(Y9, Y10, Y11) :- __evalo(Y9, Y10), __evalo(Y9, Y11).
elemo(cons(false, Q1), o).
elemo(cons(Q3, Q1), s(Q2)) :- elemo(Q1, Q2).
__evaloEvalo(Y14, Y15, Y16) :- _evalo(Y15, Y14), __evalo(Y14, Y16).
___evaloEvalo(Y17, Y18, Y19) :- _evalo(Y18, Y17), _evalo(Y19, Y17).
_elemo(cons(true, Q1), o).
_elemo(cons(Q3, Q1), s(Q2)) :- _elemo(Q1, Q2).