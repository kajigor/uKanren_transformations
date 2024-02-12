containso(cons(s(o), cons(Q1, Q2))) :- newoCono(Q1, Q2).
containso(cons(Q3, Q4)) :- _cono(Q4).
newoCono(o, cons(Q1, Q2)) :- _newoCono(Q1, Q2).
newoCono(Y3, Y4) :- __appendo2Appendo2Appendo1Cono(Y3, Y4).
_newoCono(Y7, Y8) :- appendo2Appendo2Appendo1Cono(Y7, Y8).
appendo2Appendo2Appendo1Cono(Y12, Y13) :- _cono(Y13).
appendo2Appendo2Appendo1Cono(Y12, Y13) :- _appendo2Appendo2Appendo1Cono(Y12, Y13).
_appendo2Appendo2Appendo1Cono(Y17, cons(Q1, Q2)) :- newoCono(Q1, Q2), appendo2(Y17).
appendo2(s(o)).
__appendo2Appendo2Appendo1Cono(Y26, Y27) :- _cono(Y27).
__appendo2Appendo2Appendo1Cono(Y26, cons(Q1, Q2)) :- newoCono(Q1, Q2), appendo2(Y26).
_cono(cons(s(o), cons(Q1, Q2))) :- newoCono(Q1, Q2).
_cono(cons(Q3, Q4)) :- _cono(Q4).