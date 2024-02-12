sorto(cons(zero, Q1)) :- sortoAppendoAppendo(Q2, Q3, Q1), __sorto(Q4, Q3), __sorto(Q5, Q2).
sortoAppendoAppendo(Y1, Y2, cons(zero, Q1)) :- _appendo(Q1, succ(zero), Y2, Y1).
sortoAppendoAppendo(Y1, Y2, Y6) :- splitoAppendo(Q2, Q3, Q4, Q5, Q6, Q7), appendoAppendo(Y1, Y2, Q2, Y6), __sorto(Q5, Q6), __sorto(Q8, Q7).
appendoAppendo(Y10, Y11, nil, Y14) :- _appendo(Y14, succ(zero), cons(zero, Y11), Y10).
appendoAppendo(Y10, Y11, cons(Q1, Q3), cons(Q1, Q2)) :- appendoAppendo(Y10, Y11, Q3, Q2).
splitoAppendo(Y15, Y16, nil, nil, Y20, Y21) :- _appendo(Y15, Y16, Y20, Y21).
splitoAppendo(Y15, Y16, cons(Q1, Q3), cons(Q1, Q2), Y20, Y21) :- splitoAppendo(Y15, Y16, Q3, Q2, Y20, Y21), le(Y16, Q1).
splitoAppendo(Y15, Y16, cons(Q1, Q3), Y18, Y20, Y21) :- splitoAppendo(Y15, Y16, Q3, Y18, Y20, Y21), gt(Y16, Q1).
_appendo(cons(Y23, Y25), Y23, nil, Y25).
_appendo(cons(Q1, Q3), Y23, cons(Q1, Q2), Y25) :- _appendo(Q3, Y23, Q2, Y25).
le(Y26, zero).
le(succ(Q2), succ(Q1)) :- le(Q2, Q1).
gt(zero, succ(Q1)).
gt(succ(Q3), succ(Q2)) :- gt(Q3, Q2).
__sorto(nil, nil).
__sorto(cons(Q1, Q2), Y33) :- splitoAppendo(Y33, Q1, Q2, Q3, Q4, Q5), __sorto(Q3, Q4), __sorto(Q6, Q5).