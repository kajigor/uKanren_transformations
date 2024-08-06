evalo(conj(Q1, Q2), Y1) :- _evalo(Y1, Q1, true), _evalo(Y1, Q2, true).
evalo(disj(Q1, Q2), Y1) :- _nando(false, Q4), _evalo(Y1, Q1, Q4), _nando(false, Q3), _evalo(Y1, Q2, Q3).
evalo(disj(Q1, Q2), Y1) :- _nando(false, Q4), _evalo(Y1, Q1, Q4), _nando(true, Q3), _evalo(Y1, Q2, Q3).
evalo(disj(Q1, Q2), Y1) :- _nando(true, Q4), _evalo(Y1, Q1, Q4), _nando(false, Q3), _evalo(Y1, Q2, Q3).
evalo(neg(Q1), Y1) :- __nando(Q4), _evalo(Y1, Q1, Q4).
evalo(var(Q7), cons(pair(Q7, true), Q8)).
evalo(var(Q7), cons(pair(Q9, Q10), Q8)) :- _assoco(Q8, Q7).
evalo(lit(true), Y1).
nando(false, false, true).
nando(false, true, true).
nando(true, false, true).
nando(true, true, false).
_evalo(Y5, conj(Q1, Q2), true) :- nando(Q3, Q4, false), _evalo(Y5, Q1, Q3), _evalo(Y5, Q2, Q4).
_evalo(Y5, conj(Q1, Q2), false) :- nando(Q3, Q4, true), _evalo(Y5, Q1, Q3), _evalo(Y5, Q2, Q4).
_evalo(Y5, disj(Q1, Q2), Y7) :- _nando(Q6, Q3), _evalo(Y5, Q1, Q3), nando(Q6, Q7, Y7), _nando(Q7, Q4), _evalo(Y5, Q2, Q4).
_evalo(Y5, neg(Q1), Y7) :- _nando(Y7, Q3), _evalo(Y5, Q1, Q3).
_evalo(cons(pair(Q8, Y7), Q9), var(Q8), Y7).
_evalo(cons(pair(Q10, Q11), Q9), var(Q8), Y7) :- assoco(Q9, Y7, Q8).
_evalo(Y5, lit(Y7), Y7).
_nando(true, false).
_nando(false, true).
assoco(cons(pair(Y12, Y11), Q1), Y11, Y12).
assoco(cons(pair(Q2, Q3), Q1), Y11, Y12) :- assoco(Q1, Y11, Y12).
__nando(false).
_assoco(cons(pair(Y15, true), Q1), Y15).
_assoco(cons(pair(Q2, Q3), Q1), Y15) :- _assoco(Q1, Y15).