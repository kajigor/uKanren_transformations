solve(Y0, Y1, Y2) :- _contrapositiveProveall(Y0, Y1, Y2, nil).
_contrapositiveProveall(Y15, Y16, Y17, Y19) :- _input_clauseProveall(Y15, Y16, Y17, Y19).
_input_clauseProveall(nil, Y22, Y22, Y23).
_input_clauseProveall(cons(Q1, Q3), Y21, cons(Q1, Q2), Y23) :- _prove(Y23, Q1, Q3, Y21, Q2).
_prove(Y25, Y26, Y27, Y28, Y29) :- member(Y27, Y28, Y29, Y25).
_prove(Y25, Y26, Y27, Y28, Y29) :- _contrapositiveProveall(Y27, Y28, Y29, cons(pos(app(cons(Y26, Y27), Y28, cons(Y26, Y29))), Y25)).
member(Y30, Y31, Y32, cons(neg(app(Y30, Y31, Y32)), Q1)).
member(Y30, Y31, Y32, cons(Q2, Q3)) :- member(Y30, Y31, Y32, Q3).