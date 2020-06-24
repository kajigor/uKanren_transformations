assoco(cons(pair(Y3, integer), Q1), Y3).
assoco(cons(pair(Q2, Q3), Q1), Y3) :- assoco(Q1, Y3).
_type(iConst(Q1), Y5).
_type(var_(Q2), cons(pair(Q2, integer), Q3)).
_type(var_(Q2), cons(pair(Q4, Q5), Q3)) :- assoco(Q3, Q2).
_type(plus(Q6, Q7), Y5) :- typeType(Y5, Q6, Q7).
_type(mult(Q6, Q7), Y5) :- typeType(Y5, Q6, Q7).
_type(let_(Q8, Q9, Q10), Y5) :- __type(Y5, Q9, Q11), __type(cons(pair(Q8, Q11), Y5), Q10, integer).
_type(if_(Q12, Q13, Q14), Y5) :- ___type(Y5, Q12), _type(Q13, Y5), _type(Q14, Y5).
typeType(Y6, Y7, Y8) :- _type(Y7, Y6), _type(Y8, Y6).
__type(Y9, bConst(Q1), boolean).
__type(Y9, iConst(Q2), integer).
__type(cons(pair(Q3, Y11), Q4), var_(Q3), Y11).
__type(cons(pair(Q5, Q6), Q4), var_(Q3), Y11) :- _assoco(Q4, Y11, Q3).
__type(Y9, plus(Q7, Q8), integer) :- typeType(Y9, Q7, Q8).
__type(Y9, mult(Q7, Q8), integer) :- typeType(Y9, Q7, Q8).
__type(Y9, eq(Q9, Q10), boolean) :- _typeType(Y9, Q9, Q10).
__type(Y9, lt(Q9, Q10), boolean) :- _typeType(Y9, Q9, Q10).
__type(Y9, let_(Q11, Q12, Q13), Y11) :- __type(Y9, Q12, Q14), __type(cons(pair(Q11, Q14), Y9), Q13, Y11).
__type(Y9, if_(Q15, Q16, Q17), Y11) :- ___type(Y9, Q15), __type(Y9, Q16, Y11), __type(Y9, Q17, Y11).
_assoco(cons(pair(Y14, Y13), Q1), Y13, Y14).
_assoco(cons(pair(Q2, Q3), Q1), Y13, Y14) :- _assoco(Q1, Y13, Y14).
_typeType(Y15, Y16, Y17) :- __type(Y15, Y16, Q1), __type(Y15, Y17, Q1).
___type(Y19, bConst(Q1)).
___type(cons(pair(Q2, boolean), Q3), var_(Q2)).
___type(cons(pair(Q4, Q5), Q3), var_(Q2)) :- __assoco(Q3, Q2).
___type(Y19, eq(Q6, Q7)) :- _typeType(Y19, Q6, Q7).
___type(Y19, lt(Q6, Q7)) :- _typeType(Y19, Q6, Q7).
___type(Y19, let_(Q8, Q9, Q10)) :- __type(Y19, Q9, Q11), ___type(cons(pair(Q8, Q11), Y19), Q10).
___type(Y19, if_(Q12, Q13, Q14)) :- ___type(Y19, Q12), ___type(Y19, Q13), ___type(Y19, Q14).
__assoco(cons(pair(Y22, boolean), Q1), Y22).
__assoco(cons(pair(Q2, Q3), Q1), Y22) :- __assoco(Q1, Y22).