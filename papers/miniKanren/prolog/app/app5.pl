appendo(nil, Y2, Y2).
appendo(cons(Q1, nil), Q3, cons(Q1, Q3)).
appendo(cons(Q1, cons(Q4, nil)), Q6, cons(Q1, cons(Q4, Q6))).
appendo(cons(Q1, cons(Q4, cons(Q7, nil))), Q9, cons(Q1, cons(Q4, cons(Q7, Q9)))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, nil)))), Q12, cons(Q1, cons(Q4, cons(Q7, cons(Q10, Q12))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, nil))))), Q15, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, Q15)))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, Q17)))))), Y1, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, Q18))))))) :- _appendo(Y1, Q17, Q18).
_appendo(Y5, nil, Y5).
_appendo(Y3, cons(Q1, Q2), cons(Q1, Q3)) :- _appendo(Y3, Q2, Q3).