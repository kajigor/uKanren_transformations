checkAnswer(Y0, Y1, Y2, Y3) :- startState(Y1, Y2, Q1), calcFuel(Y0, Y1, Y2, Y3, Q1).
startState(o, Y5, st(o, Y5, nil)).
startState(s(Q2), Y5, st(o, Y5, cons(o, Q3))) :- stations(Q2, Q3).
calcFuel(nil, o, Y10, Y10, st(o, Q1, Q2)).
calcFuel(nil, s(Q4), Y10, Y10, st(s(Q3), Q1, Q2)) :- eqNat(Q4, Q3).
calcFuel(cons(left(Q7), Q6), Y8, Y9, Y10, Y11) :- checkStep(Y8, Y9, Y11, Q7), stepCalcFuel(Y8, Y9, Y11, Q6, Y10, Q7).
calcFuel(cons(right(Q8), Q6), Y8, Y9, Y10, Y11) :- checkStepStepCalcFuel(Y8, Y9, Y11, Q6, Q9, Q8), _getFuel(Y9, Y11, Q10, Q8), add(Q9, Y10, Q10).
stations(o, nil).
stations(s(Q1), cons(o, Q2)) :- stations(Q1, Q2).
eqNat(o, o).
eqNat(s(Q2), s(Q1)) :- eqNat(Q2, Q1).
checkStep(Y16, Y17, st(Q1, Q2, Q3), s(Q4)) :- goe(Q1, Q4), goe(Q2, Q4).
stepCalcFuel(Y20, Y21, Y22, Y23, Y25, Y26) :- step(Y20, Y21, Y22, Q1, Y26), _calcFuel(Y20, Y21, Y23, Q1, Y25).
goe(s(o), o).
goe(s(s(Q2)), o).
goe(s(s(Q2)), s(Q3)) :- _goe(Q2, Q3).
_goe(o, o).
_goe(s(Q1), o).
_goe(s(Q1), s(Q2)) :- _goe(Q1, Q2).
step(Y31, Y32, st(Q1, Q2, Q3), st(Q4, Q5, Q3), Y35) :- sub(Q1, Y35, Q4), sub(Q2, Y35, Q5).
_calcFuel(Y36, Y40, nil, Y39, Y40) :- isFinishState(Y36, Y39).
_calcFuel(Y36, Y37, cons(fill, Q2), Y39, Y40) :- __checkStep(Y36, Y37, Y39), __step(Y36, Y37, Y39, Q3), calcFuel(Q2, Y36, Y37, Q4, Q3), __getFuel(Y37, Y39, Q5), add(Q4, Y40, Q5).
_calcFuel(Y36, Y37, cons(pour(Q6), Q2), Y39, Y40) :- ___checkStep(Y36, Y37, Y39, Q6), ___step(Y36, Y37, Y39, Q3, Q6), calcFuel(Q2, Y36, Y37, Q4, Q3), ___getFuel(Y37, Y39, Q5, Q6), add(Q4, Y40, Q5).
sub(Y43, o, Y43).
sub(o, s(Q1), o).
sub(s(Q2), s(Q1), Y43) :- sub(Q2, Q1, Y43).
isFinishState(o, st(o, Q2, Q3)).
isFinishState(s(Q5), st(s(Q4), Q2, Q3)) :- eqNat(Q5, Q4).
add(Y51, Y51, o).
add(Y50, s(Q2), s(Q1)) :- add(Y50, Q2, Q1).
_checkStep(Y53, Y54, st(Q1, Q2, Q3), s(Q4)) :- addGoe(Y53, Q1, Q4), goe(Q2, Q4).
_step(Y57, Y58, st(Q1, Q2, Q3), st(Q4, Q5, Q3), Y61) :- add(Y61, Q4, Q1), sub(Q2, Y61, Q5).
_getFuel(Y62, Y63, o, Y65).
addGoe(Y66, Y67, Y69) :- _add(Y67, Q1, Y69), _goe(Y66, Q1).
_add(o, s(Y72), Y72).
_add(s(Q1), s(Q2), Y72) :- _add(Q1, Q2, Y72).
__checkStep(Y73, Y74, st(o, Q1, Q2)) :- _eqNat(Y74, Q1).
__checkStep(Y73, Y74, st(s(Q3), Q1, Q2)) :- _eqNat(Y74, Q1), elem(Q2, Q3).
__step(Y76, Y77, st(o, Q1, Q2), st(o, Y77, Q2)).
__step(Y76, Y77, st(s(Q3), Q1, Q2), st(s(Q3), Y77, Q4)) :- elemAdd(Q1, Q2, Q3, Q5), _goe(Q5, Y77), subSetForElem(Y77, Q2, Q3, Q5, Q4).
__step(Y76, Y77, st(s(Q3), Q1, Q2), st(s(Q3), Q5, Q6)) :- elemAdd(Q1, Q2, Q3, Q5), __goe(Y77, Q5), _setForElem(Q2, Q3, Q6).
__getFuel(Y80, st(o, Q1, Q2), Y82) :- sub(Y80, Q1, Y82).
__getFuel(Y80, st(s(Q3), Q1, Q2), o).
_eqNat(s(Q1), o).
_eqNat(o, s(Q2)).
_eqNat(s(Q3), s(Q2)) :- _eqNat(Q3, Q2).
elem(cons(s(Q1), Q2), o).
elem(cons(Q3, Q2), s(Q4)) :- elem(Q2, Q4).
elemAdd(Y88, Y89, Y90, Y92) :- _elem(Y89, Y90, Q1), add(Q1, Y92, Y88).
subSetForElem(Y93, Y94, Y95, Y96, Y97) :- sub(Y96, Y93, Q1), setForElem(Y94, Y95, Y97, Q1).
_elem(cons(Y101, Q1), o, Y101).
_elem(cons(Q2, Q1), s(Q3), Y101) :- _elem(Q1, Q3, Y101).
setForElem(cons(Q1, Q2), o, cons(Y105, Q2), Y105).
setForElem(cons(Q1, cons(Q5, Q6)), s(o), cons(Q1, cons(Q7, Q6)), Y105) :- add(Q5, Q7, Y105).
setForElem(cons(Q1, cons(Q5, Q6)), s(s(Q8)), cons(Q1, cons(Q5, Q9)), Y105) :- addForElem(Y105, Q6, Q8, Q9).
addForElem(Y106, cons(Q1, Q2), o, cons(Q3, Q2)) :- add(Q1, Q3, Y106).
addForElem(Y106, cons(Q1, Q2), s(Q4), cons(Q1, Q5)) :- addForElem(Y106, Q2, Q4, Q5).
__goe(s(Q1), o).
__goe(s(Q3), s(Q2)) :- __goe(Q3, Q2).
_setForElem(cons(Q1, Q2), o, cons(o, Q2)).
_setForElem(cons(Q1, cons(Q5, Q6)), s(o), cons(Q1, cons(Q5, Q6))).
_setForElem(cons(Q1, cons(Q5, Q6)), s(s(Q7)), cons(Q1, cons(Q5, Q8))) :- _addForElem(Q6, Q7, Q8).
_addForElem(cons(Q1, Q2), o, cons(Q1, Q2)).
_addForElem(cons(Q1, Q2), s(Q3), cons(Q1, Q4)) :- _addForElem(Q2, Q3, Q4).
___checkStep(Y118, Y119, st(s(Q1), Q2, Q3), s(Q4)) :- __eqNat(Y118, Q1), goe(Q2, Q4).
___step(Y122, Y123, st(s(Q1), Q2, Q3), st(s(Q1), Q4, Q5), Y126) :- sub(Q2, Y126, Q4), addForElem(Y126, Q3, Q1, Q5).
___getFuel(Y127, Y128, o, Y130).
__eqNat(o, Y132).
__eqNat(s(Q1), Y132) :- _eqNat(Q1, Y132).
checkStepStepCalcFuel(Y133, Y134, Y135, Y136, Y138, Y139) :- _checkStep(Y133, Y134, Y135, Y139), _step(Y133, Y134, Y135, Q1, Y139), _calcFuel(Y133, Y134, Y136, Q1, Y138).