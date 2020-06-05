add(o,Z,Z).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

greater(o,_,false).
greater(s(_),o,true).
greater(s(X),s(Y),R) :- greater(X,Y,R).

sub(X,o,X).
sub(o,s(_),o).
sub(s(X),s(Y),R) :- sub(X,Y,R).

anotherBottle(fst,snd).
anotherBottle(snd,fst).

createState(fst,X,Y,pair(X,Y)).
createState(snd,X,Y,pair(Y,X)).

first(pair(X,_),X).

second(pair(_,Y),Y).

getCapacity(Capacities, fst, Q) :- first(Capacities,Q).
getCapacity(Capacities, snd, Q) :- second(Capacities,Q).

fancyEq(o,o,true).
fancyEq(o,s(_),false).
fancyEq(s(_),o,false).
fancyEq(s(X),s(Y),R) :- fancyEq(X,Y,R).


isFinishState(pair(F,S),ReqLvl,true) :- fancyEq(F,ReqLvl,true), fancyEq(S,ReqLvl,_).
isFinishState(pair(F,S),ReqLvl,Q) :- fancyEq(F,ReqLvl,false), fancyEq(S,ReqLvl,Q).

checkAnswer(State, [], _, ReqLvl, Q) :- isFinishState(State,ReqLvl,Q).
checkAnswer(State, [X|Xs], Capacities, ReqLvl, Q1) :-
  checkStep(State, X, Capacities, true),
  doStep(State,X,Capacities,Q4),
  checkAnswer(Q4,Xs,Capacities,ReqLvl,Q1).
checkAnswer(State, [X|_], Capacities, ReqLvl, false) :- checkStep(State, X, Capacities, false).

checkAnswer0(Answer,Capacities,ReqLvl,Q) :- checkAnswer(pair(o,o), Answer, Capacities, ReqLvl, Q).


checkStep(pair(Q51,_),pair(fill,fst),_,Q48) :- fancyEq(Q51,o,Q48).
checkStep(pair(_,Q51),pair(fill,snd),_,Q48) :- fancyEq(Q51,o,Q48).

checkStep(pair(Q56,_),pair(empty,fst),Capacities,Q48) :- getCapacity(Capacities, fst, Q57), fancyEq(Q56, Q57, Q48).
checkStep(pair(_,Q56),pair(empty,snd),Capacities,Q48) :- getCapacity(Capacities, snd, Q57), fancyEq(Q56, Q57, Q48).

checkStep(pair(Q72,Q77),pair(pour,fst),Capacities,false) :- fancyEq(Q72, o, true), anotherBottle(fst,Q83), getCapacity(Capacities, Q83, Q78), fancyEq(Q77,Q78,Q67).
checkStep(pair(Q77,Q72),pair(pour,snd),Capacities,false) :- fancyEq(Q72, o, true), anotherBottle(snd,Q83), getCapacity(Capacities, Q83, Q78), fancyEq(Q77,Q78,Q67).

checkStep(pair(Q72,Q77),pair(pour,fst),Capacities,false) :- fancyEq(Q72, o, false), anotherBottle(fst,Q83), getCapacity(Capacities, Q83, Q78), fancyEq(Q77,Q78,true).
checkStep(pair(Q77,Q72),pair(pour,snd),Capacities,false) :- fancyEq(Q72, o, false), anotherBottle(snd,Q83), getCapacity(Capacities, Q83, Q78), fancyEq(Q77,Q78,true).

checkStep(pair(Q72,Q77),pair(pour,fst),Capacities,true) :- fancyEq(Q72, o, false), anotherBottle(fst,Q83), getCapacity(Capacities, Q83, Q78), fancyEq(Q77,Q78,false).
checkStep(pair(Q77,Q72),pair(pour,snd),Capacities,true) :- fancyEq(Q72, o, false), anotherBottle(snd,Q83), getCapacity(Capacities, Q83, Q78), fancyEq(Q77,Q78,false).


doStep(pair(_,Q19), pair(fill,fst), Capacities, Q15) :- getCapacity(Capacities, fst, Q18), createState(fst, Q18, Q19, Q15).
doStep(pair(Q19,_), pair(fill,snd), Capacities, Q15) :- getCapacity(Capacities, snd, Q18), createState(snd, Q18, Q19, Q15).

doStep(pair(_,Q24), pair(empty,fst), _, Q15) :- createState(fst, o, Q24, Q15).
doStep(pair(Q24,_), pair(empty,snd), _, Q15) :- createState(snd, o, Q24, Q15).

doStep(pair(F,S), pair(pour,B), Capacities, Q15) :-
  add(F,S,Q43),
  anotherBottle(B,Q46),
  getCapacity(Capacities, Q46, Q44),
  greater(Q43,Q44,true),
  add(F,S,Q34),
  anotherBottle(B,Q37),
  getCapacity(Capacities, Q37, Q35),
  sub(Q34, Q35, Q31),
  anotherBottle(B, Q39),
  getCapacity(Capacities, Q39, Q32),
  createState(B, Q31, Q32, Q15).
doStep(pair(F,S), pair(pour,B), Capacities, Q15) :-
  add(F,S,Q43),
  anotherBottle(B,Q46),
  getCapacity(Capacities, Q46, Q44),
  greater(Q43,Q44,false),
  add(F,S,Q41),
  createState(B,o,Q41,Q15).

capacities1(pair(s(s(s(s(o)))),s(s(s(s(s(s(s(s(s(o))))))))))).

topLevel(Res) :- capacities1(Cap), checkAnswer0(Res, Cap, s(s(s(s(s(s(s(o))))))),true).

