add(o, Y, Y).
add(s(X1), Y, s(Z1)) :- add(X1, Y, Z1).
goe(o, o, true).
goe(o, s(Q189), false).
goe(s(X), o, true).
goe(s(X), s(Y), Q186) :- goe(X, Y, Q186).
sub(Q182, o, Q182).
sub(o, s(Y), o).
sub(s(X), s(Y), Q182) :- sub(X, Y, Q182).
elem(cons(Q179, Xs), o, Q179).
elem(cons(X, Xs), s(M), Q179) :- elem(Xs, M, Q179).
eqNat(o, o, true).
eqNat(o, s(Q175), false).
eqNat(s(X), o, false).
eqNat(s(X), s(Y), Q172) :- eqNat(X, Y, Q172).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, false) :- goe(Pos, D, false), goe(Fuel, D, false), eqNat(D, o, true).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, false) :- goe(Pos, D, true), goe(Fuel, D, false), eqNat(D, o, true).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, false) :- goe(Pos, D, false), goe(Fuel, D, true), eqNat(D, o, true).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, false) :- goe(Pos, D, true), goe(Fuel, D, true), eqNat(D, o, true).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, false) :- goe(Pos, D, false), goe(Fuel, D, false), eqNat(D, o, false).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, false) :- goe(Pos, D, true), goe(Fuel, D, false), eqNat(D, o, false).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, false) :- goe(Pos, D, false), goe(Fuel, D, true), eqNat(D, o, false).
checkStep(left(D), st(Pos, Fuel, Sts), Len, Cop, true) :- goe(Pos, D, true), goe(Fuel, D, true), eqNat(D, o, false).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, false) :- add(Pos, D, Q109), goe(Len, Q109, false), goe(Fuel, D, false), eqNat(D, o, true).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, false) :- add(Pos, D, Q109), goe(Len, Q109, true), goe(Fuel, D, false), eqNat(D, o, true).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, false) :- add(Pos, D, Q109), goe(Len, Q109, false), goe(Fuel, D, true), eqNat(D, o, true).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, false) :- add(Pos, D, Q109), goe(Len, Q109, true), goe(Fuel, D, true), eqNat(D, o, true).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, false) :- add(Pos, D, Q109), goe(Len, Q109, false), goe(Fuel, D, false), eqNat(D, o, false).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, false) :- add(Pos, D, Q109), goe(Len, Q109, true), goe(Fuel, D, false), eqNat(D, o, false).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, false) :- add(Pos, D, Q109), goe(Len, Q109, false), goe(Fuel, D, true), eqNat(D, o, false).
checkStep(right(D), st(Pos, Fuel, Sts), Len, Cop, true) :- add(Pos, D, Q109), goe(Len, Q109, true), goe(Fuel, D, true), eqNat(D, o, false).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, false) :- eqNat(Pos, Len, true), eqNat(Pos, o, true), eqNat(F, o, true), goe(Fuel, F, Q142).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, false) :- eqNat(Pos, Len, true), eqNat(Pos, o, true), eqNat(F, o, false), goe(Fuel, F, Q142).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, false) :- eqNat(Pos, Len, true), eqNat(Pos, o, false), eqNat(F, o, true), goe(Fuel, F, Q142).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, false) :- eqNat(Pos, Len, true), eqNat(Pos, o, false), eqNat(F, o, false), goe(Fuel, F, Q142).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, false) :- eqNat(Pos, Len, false), eqNat(Pos, o, true), eqNat(F, o, true), goe(Fuel, F, Q142).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, false) :- eqNat(Pos, Len, false), eqNat(Pos, o, true), eqNat(F, o, false), goe(Fuel, F, Q142).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, false) :- eqNat(Pos, Len, false), eqNat(Pos, o, false), eqNat(F, o, true), goe(Fuel, F, Q142).
checkStep(pour(F), st(Pos, Fuel, Sts), Len, Cop, Q142) :- eqNat(Pos, Len, false), eqNat(Pos, o, false), eqNat(F, o, false), goe(Fuel, F, Q142).
checkStep(fill, st(o, Fuel, Sts), Len, Cop, false) :- eqNat(Fuel, Cop, true).
checkStep(fill, st(o, Fuel, Sts), Len, Cop, true) :- eqNat(Fuel, Cop, false).
checkStep(fill, st(s(X), Fuel, Sts), Len, Cop, false) :- eqNat(Fuel, Cop, true), elem(Sts, X, Q170), eqNat(Q170, o, true).
checkStep(fill, st(s(X), Fuel, Sts), Len, Cop, false) :- eqNat(Fuel, Cop, true), elem(Sts, X, Q170), eqNat(Q170, o, false).
checkStep(fill, st(s(X), Fuel, Sts), Len, Cop, false) :- eqNat(Fuel, Cop, false), elem(Sts, X, Q170), eqNat(Q170, o, true).
checkStep(fill, st(s(X), Fuel, Sts), Len, Cop, true) :- eqNat(Fuel, Cop, false), elem(Sts, X, Q170), eqNat(Q170, o, false).
addForElem(cons(X, Xs), o, V, cons(Q81, Xs)) :- add(V, X, Q81).
addForElem(cons(X, Xs), s(M), V, cons(X, Q83)) :- addForElem(Xs, M, V, Q83).
setForElem(cons(X, Xs), o, V, cons(V, Xs)).
setForElem(cons(X, Xs), s(M), V, cons(X, Q77)) :- addForElem(Xs, M, V, Q77).
step(left(D), st(Pos, Fuel, Sts), Len, Cop, st(Q52, Q53, Sts)) :- sub(Pos, D, Q52), sub(Fuel, D, Q53).
step(right(D), st(Pos, Fuel, Sts), Len, Cop, st(Q55, Q56, Sts)) :- add(Pos, D, Q55), sub(Fuel, D, Q56).
step(pour(F), st(s(X), Fuel, Sts), Len, Cop, st(s(X), Q59, Q60)) :- sub(Fuel, F, Q59), addForElem(Sts, X, F, Q60).
step(fill, st(o, Fuel, Sts), Len, Cop, st(o, Cop, Sts)).
step(fill, st(s(X), Fuel, Sts), Len, Cop, st(s(X), Cop, Q68)) :- elem(Sts, X, StationFuel), add(Fuel, StationFuel, TotalFuel), goe(TotalFuel, Cop, true), sub(TotalFuel, Cop, Q70), setForElem(Sts, X, Q70, Q68).
step(fill, st(s(X), Fuel, Sts), Len, Cop, st(s(X), TotalFuel, Q72)) :- elem(Sts, X, StationFuel), add(Fuel, StationFuel, TotalFuel), goe(TotalFuel, Cop, false), setForElem(Sts, X, o, Q72).
isFinishState(st(Pos, Fuel, Sts), Len, Q49) :- eqNat(Pos, Len, Q49).
getFuel(left(D), State, Cop, o).
getFuel(right(D), State, Cop, o).
getFuel(pour(F), State, Cop, o).
getFuel(fill, st(o, Fuel, Sts), Cop, Q42) :- sub(Cop, Fuel, Q42).
getFuel(fill, st(s(X), Fuel, Sts), Cop, o).
isMove(left(Q35), true).
isMove(right(Q37), true).
isMove(fill, false).
isMove(pour(Q40), false).
eqBool(true, Q30, Q30).
eqBool(false, true, false).
eqBool(false, false, true).
startState(Len, Cop, st(o, Cop, Q27)) :- stations(Len, Q27).
stations(o, nil).
stations(s(M), cons(o, Q25)) :- stations(M, Q25).
calcFuel(State, nil, Len, Cop, PrevIsMove, some(Cop)) :- isFinishState(State, Len, true).
calcFuel(State, nil, Len, Cop, PrevIsMove, none) :- isFinishState(State, Len, false).
calcFuel(State, cons(X, Xs), Len, Cop, PrevIsMove, none) :- isMove(X, CurrIsMove), eqBool(PrevIsMove, CurrIsMove, true).
calcFuel(State, cons(X, Xs), Len, Cop, PrevIsMove, none) :- isMove(X, CurrIsMove), eqBool(PrevIsMove, CurrIsMove, false), checkStep(X, State, Len, Cop, true), step(X, State, Len, Cop, Q20), calcFuel(Q20, Xs, Len, Cop, CurrIsMove, none).
calcFuel(State, cons(X, Xs), Len, Cop, PrevIsMove, some(Q16)) :- isMove(X, CurrIsMove), eqBool(PrevIsMove, CurrIsMove, false), checkStep(X, State, Len, Cop, true), step(X, State, Len, Cop, Q20), calcFuel(Q20, Xs, Len, Cop, CurrIsMove, some(Res)), getFuel(X, State, Cop, Q18), add(Q18, Res, Q16).
calcFuel(State, cons(X, Xs), Len, Cop, PrevIsMove, none) :- isMove(X, CurrIsMove), eqBool(PrevIsMove, CurrIsMove, false), checkStep(X, State, Len, Cop, false).
checkAnswer(Answer, Len, Cop, Q1) :- startState(Len, Cop, Q0), calcFuel(Q0, Answer, Len, Cop, false, Q1).