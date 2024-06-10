help(nil, Y2, Y2) :- _reverso(Y2, Y2).
help(cons(Q1, Q3), Y1, cons(Q1, Q2)) :- _appendoReversoAppendo(Y1, Q3, Q2, Q1, Y1).
_reversoAppendoAppendo(nil, Y15, Y15, Y17, Y18, Y19) :- appendo(cons(Y18, Y19), Y17, nil).
_reversoAppendoAppendo(cons(Q1, Q2), Y14, Y15, Y17, Y18, Y19) :- __reversoAppendo(Q2, Q1, Q3), appendoAppendo(Q3, Y14, Y15, Y17, Y18, Y19).
_reverso(nil, nil).
_reverso(cons(Q1, Q2), Y21) :- __reversoAppendo(Q2, Q1, Y21).
appendoAppendo(nil, Y24, Y24, Y26, Y27, Y28) :- appendo(cons(Y27, Y28), Y26, nil).
appendoAppendo(cons(Y24, Q1), Y23, Y24, Y26, Y27, Y28) :- __appendoAppendo(Y23, Y26, Q1, cons(Y27, Y28)).
__appendoAppendo(Y35, Y36, nil, Y39) :- appendo(Y39, Y36, cons(Y35, nil)).
__appendoAppendo(Y35, Y36, cons(Q1, Q3), cons(Q1, Q2)) :- __appendoAppendo(Y35, Y36, Q3, Q2).
appendo(cons(Y41, nil), Y41, nil).
appendo(cons(Q1, Q3), Y41, cons(Q1, Q2)) :- appendo(Q3, Y41, Q2).
__reversoAppendo(nil, Y53, Y54) :- appendo(Y54, Y53, nil).
__reversoAppendo(cons(Q1, Q2), Y53, Y54) :- __reversoAppendo(Q2, Q1, Q3), appendo(Y54, Y53, Q3).
_appendoReversoAppendo(Y57, nil, Y57, Y59, Y60) :- __reversoAppendo(Y57, Y59, Y60).
_appendoReversoAppendo(Y55, Y56, cons(Q1, Q2), Y59, Y60) :- _appendoReversoAppendo(Y55, Q3, Q2, Q1, Q4), _appendoReversoAppendo(Y55, Q3, Q2, Q1, Q4), appendo(Y60, Y59, Q4).