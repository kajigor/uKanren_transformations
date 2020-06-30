eq_nat(z, z).
eq_nat(s(Q1), s(Q2)) :- eq_nat(Q1, Q2).
forall2(Y5, nil, nil).
forall2(Y5, cons(Q1, Q2), cons(Q3, Q4)) :- _check_uni(Y5, Q1, Q3), forall2(Y5, Q2, Q4).
_check_uni(Y8, constr(Q1, Q2), constr(Q3, Q4)) :- eq_nat(Q1, Q3), forall2(Y8, Q2, Q4).
_check_uni(Y8, var_(Q5), constr(Q6, Q7)) :- get_term(Y8, Q5, Q8), _check_uni(Y8, Q8, constr(Q6, Q7)).
_check_uni(Y8, constr(Q9, Q10), var_(Q11)) :- get_term(Y8, Q11, Q12), _check_uni(Y8, constr(Q9, Q10), Q12).
_check_uni(Y8, var_(Q13), var_(Q14)) :- get_term(Y8, Q13, Q15), _check_uni(Y8, Q15, var_(Q14)).
_check_uni(Y8, var_(Q13), var_(Q14)) :- get_termGet_term(Y8, Q13, Q14), eq_nat(Q13, Q14).
get_term(cons(some(Y13), Q1), z, Y13).
get_term(cons(Q2, Q1), s(Q3), Y13) :- get_term(Q1, Q3, Y13).
get_termGet_term(Y14, Y15, Y16) :- _get_term(Y14, Y15), _get_term(Y14, Y16).
_get_term(nil, Y18).
_get_term(cons(none, Q1), z).
_get_term(cons(Q2, Q1), s(Q3)) :- _get_term(Q1, Q3).