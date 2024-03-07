fail() :- fail().
eqNat(o, o, trueo).
eqNat(o, s(R1), falso).
eqNat(s(X), o, falso).
eqNat(s(X), s(Y), R) :- eqNat(X, Y, R).
add(o, R, R).
add(s(X), B, R) :- add(X, s(B), R).
greater(o, B, falso).
greater(s(X), o, trueo).
greater(s(X), s(Y), R) :- greater(X, Y, R).
sub(R, o, R).
sub(o, s(Y), o).
sub(s(X), s(Y), R) :- sub(X, Y, R).
anotherBottle(fst, snd).
anotherBottle(snd, fst).
createState(fst, Lvl1, Lvl2, pair(Lvl1, Lvl2)).
createState(snd, Lvl1, Lvl2, pair(Lvl2, Lvl1)).
capacities(fst, s(s(s(s(o))))).
capacities(snd, s(s(s(s(s(s(s(s(s(o)))))))))).
checkStep(pair(Lvl1, Lvl2), Step0, IsStepValid) :- eqNat(Lvl1, o, IsStepValid).
checkStep(pair(Lvl1, Lvl2), Step0, IsStepValid) :- capacities(fst, Q44), eqNat(Lvl1, Q44, IsStepValid).
checkStep(pair(Lvl1, Lvl2), pair(pour, fst), falso) :- anotherBottle(fst, B_0), eqNat(Lvl1, o, trueo), capacities(B_0, B_0Cap), eqNat(Lvl2, B_0Cap, Lvl2IsCap).
checkStep(pair(Lvl1, Lvl2), pair(pour, fst), falso) :- anotherBottle(fst, B_0), eqNat(Lvl1, o, falso), capacities(B_0, B_0Cap), eqNat(Lvl2, B_0Cap, trueo).
checkStep(pair(Lvl1, Lvl2), pair(pour, fst), trueo) :- anotherBottle(fst, B_0), eqNat(Lvl1, o, falso), capacities(B_0, B_0Cap), eqNat(Lvl2, B_0Cap, falso).
checkStep(pair(Lvl2, Lvl1), Step0, IsStepValid) :- eqNat(Lvl1, o, IsStepValid).
checkStep(pair(Lvl2, Lvl1), Step0, IsStepValid) :- capacities(snd, Q44), eqNat(Lvl1, Q44, IsStepValid).
checkStep(pair(Lvl2, Lvl1), pair(pour, snd), falso) :- anotherBottle(snd, B_0), eqNat(Lvl1, o, trueo), capacities(B_0, B_0Cap), eqNat(Lvl2, B_0Cap, Lvl2IsCap).
checkStep(pair(Lvl2, Lvl1), pair(pour, snd), falso) :- anotherBottle(snd, B_0), eqNat(Lvl1, o, falso), capacities(B_0, B_0Cap), eqNat(Lvl2, B_0Cap, trueo).
checkStep(pair(Lvl2, Lvl1), pair(pour, snd), trueo) :- anotherBottle(snd, B_0), eqNat(Lvl1, o, falso), capacities(B_0, B_0Cap), eqNat(Lvl2, B_0Cap, falso).
doStep(pair(F, Lvl2), pair(fill, fst), State) :- capacities(fst, Cap), createState(fst, Cap, Lvl2, State).
doStep(pair(F, Lvl2), pair(empt, fst), State) :- createState(fst, o, Lvl2, State).
doStep(pair(F, Lvl2), pair(pour, fst), State) :- add(F, Lvl2, Sum), anotherBottle(fst, B_0), capacities(B_0, Cap2), greater(Sum, Cap2, trueo), sub(Sum, Cap2, Diff), createState(fst, Diff, Cap2, State).
doStep(pair(F, Lvl2), pair(pour, fst), State) :- add(F, Lvl2, Sum), anotherBottle(fst, B_0), capacities(B_0, Cap2), greater(Sum, Cap2, falso), createState(fst, o, Sum, State).
doStep(pair(Lvl2, S), pair(fill, snd), State) :- capacities(snd, Cap), createState(snd, Cap, Lvl2, State).
doStep(pair(Lvl2, S), pair(empt, snd), State) :- createState(snd, o, Lvl2, State).
doStep(pair(Lvl2, S), pair(pour, snd), State) :- add(Lvl2, S, Sum), anotherBottle(snd, B_0), capacities(B_0, Cap2), greater(Sum, Cap2, trueo), sub(Sum, Cap2, Diff), createState(snd, Diff, Cap2, State).
doStep(pair(Lvl2, S), pair(pour, snd), State) :- add(Lvl2, S, Sum), anotherBottle(snd, B_0), capacities(B_0, Cap2), greater(Sum, Cap2, falso), createState(snd, o, Sum, State).
isFinishState(pair(F, S), ReqLvl, trueo) :- eqNat(F, ReqLvl, trueo), eqNat(S, ReqLvl, IsSReq).
isFinishState(pair(F, S), ReqLvl, IsSReq) :- eqNat(F, ReqLvl, falso), eqNat(S, ReqLvl, IsSReq).
checkAnswer(Answer, ReqLvl, R) :- checkAnswer_0(pair(o, o), Answer, ReqLvl, R).
checkAnswer_0(State0, nil, ReqLvl, Result) :- isFinishState(State0, ReqLvl, Result).
checkAnswer_0(State0, cons(X, Xs), ReqLvl, Result) :- checkStep(State0, X, trueo), doStep(State0, X, State_0), checkAnswer_0(State_0, Xs, ReqLvl, Result).
checkAnswer_0(State0, cons(X, Xs), ReqLvl, falso) :- checkStep(State0, X, falso).