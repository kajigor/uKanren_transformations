sorto(Y0) :- splitoSplitoSortoAppendoAppendo(Y0, Q1, Q2), __sorto(Q3, Q2), __sorto(Q4, Q1).
splitoSplitoSortoAppendoAppendo(Y1, Y4, Y9) :- _splitoSplitoSortoAppendoAppendo(Y1, Y4, Y9).
_splitoSplitoSortoAppendoAppendo(Y10, Y12, Y17) :- __splitoSplitoSortoAppendoAppendo(Y10, Y12, Y17).
__splitoSplitoSortoAppendoAppendo(Y19, Y21, Y25) :- splitoSortoAppendoAppendo(Y19, Y21, Y25).
splitoSortoAppendoAppendo(Y28, Y30, Y34) :- _splitoSortoAppendoAppendo(Y28, Y30, Y34).
_splitoSortoAppendoAppendo(cons(zero, Q1), Y37, Y40) :- sortoAppendoAppendo(Y37, Y40, Q1).
sortoAppendoAppendo(Y42, Y43, cons(zero, Q1)) :- _appendo(Q1, succ(zero), Y43, Y42).
sortoAppendoAppendo(Y42, Y43, Y47) :- splitoAppendo(Q2, Q3, Q4, Q5, Q6, Q7), appendoAppendo(Y42, Y43, Q2, Y47), __sorto(Q5, Q6), __sorto(Q8, Q7).
appendoAppendo(Y51, Y52, nil, Y55) :- _appendo(Y55, succ(zero), cons(zero, Y52), Y51).
appendoAppendo(Y51, Y52, cons(Q1, Q3), cons(Q1, Q2)) :- appendoAppendo(Y51, Y52, Q3, Q2).
splitoAppendo(Y56, Y57, nil, nil, Y61, Y62) :- _appendo(Y56, Y57, Y61, Y62).
splitoAppendo(Y56, Y57, cons(Q1, Q3), cons(Q1, Q2), Y61, Y62) :- splitoAppendo(Y56, Y57, Q3, Q2, Y61, Y62), le(Y57, Q1).
splitoAppendo(Y56, Y57, cons(Q1, Q3), Y59, Y61, Y62) :- splitoAppendo(Y56, Y57, Q3, Y59, Y61, Y62), gt(Y57, Q1).
_appendo(cons(Y64, Y66), Y64, nil, Y66).
_appendo(cons(Q1, Q3), Y64, cons(Q1, Q2), Y66) :- _appendo(Q3, Y64, Q2, Y66).
le(Y67, zero).
le(succ(Q2), succ(Q1)) :- le(Q2, Q1).
gt(zero, succ(Q1)).
gt(succ(Q3), succ(Q2)) :- gt(Q3, Q2).
__sorto(nil, nil).
__sorto(cons(Q1, Q2), Y74) :- splitoAppendo(Y74, Q1, Q2, Q3, Q4, Q5), __sorto(Q3, Q4), __sorto(Q6, Q5).