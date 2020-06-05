add(o, Y, Y).
add(s(X'), Y, s(Z')) :- add(X', Y, Z').
greater(o, B, false).
greater(s(X), o, true).
greater(s(X), s(Y), Q109) :- greater(X, Y, Q109).
sub(Q105, o, Q105).
sub(o, s(Y), o).
sub(s(X), s(Y), Q105) :- sub(X, Y, Q105).
anotherBottle(fst, snd).
anotherBottle(snd, fst).
createState(fst, Lvl1, Lvl2, pair(Lvl1, Lvl2)).
createState(snd, Lvl1, Lvl2, pair(Lvl2, Lvl1)).
fst_0(pair(Q96, Q97), Q96).
snd_0(pair(Q94, Q93), Q93).
get_capacity(Capacities, fst, Q92) :- fst_0(Capacities, Q92).
get_capacity(Capacities, snd, Q92) :- snd_0(Capacities, Q92).
fancyEq(o, o, true).
fancyEq(o, s(Q88), false).
fancyEq(s(X), o, false).
fancyEq(s(X), s(Y), Q85) :- fancyEq(X, Y, Q85).
checkStep(pair(Q51, S), pair(fill, fst), Capacities, Q48) :- fancyEq(Q51, o, Q48).
checkStep(pair(F, Q51), pair(fill, snd), Capacities, Q48) :- fancyEq(Q51, o, Q48).
checkStep(pair(Q56, S), pair(empty, fst), Capacities, Q48) :- get_capacity(Capacities, fst, Q57), fancyEq(Q56, Q57, Q48).
checkStep(pair(F, Q56), pair(empty, snd), Capacities, Q48) :- get_capacity(Capacities, snd, Q57), fancyEq(Q56, Q57, Q48).
checkStep(pair(Q72, Q77), pair(pour, fst), Capacities, false) :- fancyEq(Q72, o, true), anotherBottle(fst, Q83), get_capacity(Capacities, Q83, Q78), fancyEq(Q77, Q78, Q67).
checkStep(pair(Q72, Q77), pair(pour, fst), Capacities, false) :- fancyEq(Q72, o, false), anotherBottle(fst, Q83), get_capacity(Capacities, Q83, Q78), fancyEq(Q77, Q78, true).
checkStep(pair(Q72, Q77), pair(pour, fst), Capacities, true) :- fancyEq(Q72, o, false), anotherBottle(fst, Q83), get_capacity(Capacities, Q83, Q78), fancyEq(Q77, Q78, false).
checkStep(pair(Q77, Q72), pair(pour, snd), Capacities, false) :- fancyEq(Q72, o, true), anotherBottle(snd, Q83), get_capacity(Capacities, Q83, Q78), fancyEq(Q77, Q78, Q67).
checkStep(pair(Q77, Q72), pair(pour, snd), Capacities, false) :- fancyEq(Q72, o, false), anotherBottle(snd, Q83), get_capacity(Capacities, Q83, Q78), fancyEq(Q77, Q78, true).
checkStep(pair(Q77, Q72), pair(pour, snd), Capacities, true) :- fancyEq(Q72, o, false), anotherBottle(snd, Q83), get_capacity(Capacities, Q83, Q78), fancyEq(Q77, Q78, false).
doStep(pair(F, Q19), pair(fill, fst), Capacities, Q15) :- get_capacity(Capacities, fst, Q18), createState(fst, Q18, Q19, Q15).
doStep(pair(Q19, S), pair(fill, snd), Capacities, Q15) :- get_capacity(Capacities, snd, Q18), createState(snd, Q18, Q19, Q15).
doStep(pair(F, Q24), pair(empty, fst), Capacities, Q15) :- createState(fst, o, Q24, Q15).
doStep(pair(Q24, S), pair(empty, snd), Capacities, Q15) :- createState(snd, o, Q24, Q15).
doStep(pair(F, S), pair(pour, B), Capacities, Q15) :- add(F, S, Q43), anotherBottle(B, Q46), get_capacity(Capacities, Q46, Q44), greater(Q43, Q44, true), add(F, S, Q34), anotherBottle(B, Q37), get_capacity(Capacities, Q37, Q35), sub(Q34, Q35, Q31), anotherBottle(B, Q39), get_capacity(Capacities, Q39, Q32), createState(B, Q31, Q32, Q15).
doStep(pair(F, S), pair(pour, B), Capacities, Q15) :- add(F, S, Q43), anotherBottle(B, Q46), get_capacity(Capacities, Q46, Q44), greater(Q43, Q44, false), add(F, S, Q41), createState(B, o, Q41, Q15).
isFinishState(pair(F, S), ReqLvl, true) :- fancyEq(F, ReqLvl, true), fancyEq(S, ReqLvl, Q10).
isFinishState(pair(F, S), ReqLvl, Q10) :- fancyEq(F, ReqLvl, false), fancyEq(S, ReqLvl, Q10).
checkAnswer(Answer, Capacities, ReqLvl, Q7) :- checkAnswer_0(pair(o, o), Answer, Capacities, ReqLvl, Q7).
checkAnswer_0(State0, nil, Capacities, ReqLvl, Q1) :- isFinishState(State0, ReqLvl, Q1).
checkAnswer_0(State0, cons(X, Xs), Capacities, ReqLvl, Q1) :- checkStep(State0, X, Capacities, true), doStep(State0, X, Capacities, Q4), checkAnswer_0(Q4, Xs, Capacities, ReqLvl, Q1).
checkAnswer_0(State0, cons(X, Xs), Capacities, ReqLvl, false) :- checkStep(State0, X, Capacities, false).
capacities1(pair(s(s(s(s(o)))), s(s(s(s(s(s(s(s(s(o))))))))))).