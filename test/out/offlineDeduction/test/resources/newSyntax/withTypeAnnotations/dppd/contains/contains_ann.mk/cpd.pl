containso(cons(s(o), cons(Q1, Q2))) :- newoCono(Q1, Q2).
containso(cons(Q3, Q4)) :- _cono(Q4).
newoCono(o, cons(Q1, Q2)) :- _newoCono(Q1, Q2).
newoCono(Y3, Y4) :- __appendoAppendoAppendoCono(Y3, Y4).
_newoCono(Y7, Y8) :- appendoAppendoAppendoCono(Y7, Y8).
appendoAppendoAppendoCono(Y12, Y13) :- _cono(Y13).
appendoAppendoAppendoCono(Y12, Y13) :- _appendoAppendoAppendoCono(Y12, Y13).
_appendoAppendoAppendoCono(Y17, cons(Q1, Q2)) :- newoCono(Q1, Q2), appendo(Y17).
appendo(s(o)).
__appendoAppendoAppendoCono(Y26, Y27) :- _cono(Y27).
__appendoAppendoAppendoCono(Y26, cons(Q1, Q2)) :- newoCono(Q1, Q2), appendo(Y26).
_cono(cons(s(o), cons(Q1, Q2))) :- newoCono(Q1, Q2).
_cono(cons(Q3, Q4)) :- _cono(Q4).