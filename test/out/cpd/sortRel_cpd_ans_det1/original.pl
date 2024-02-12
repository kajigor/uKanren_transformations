fail() :- fail().
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y1, Y2, Y3, Y4) :- minmaxoMinmaxoMinmaxo______minmaxo(Y1, Y2, Y3, Y4).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y1, Y2, Y3, Y4) :- _minmaxoMinmaxoMinmaxo__________minmaxo(Y1, Y2, Y3, Y4).
minmaxoMinmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8) :- minmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8).
minmaxoMinmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8) :- _minmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8).
minmaxoMinmaxo______minmaxo(Y10, Y11, Y12, Y13) :- leo______minmaxo(Y10, Y11), minmaxo(Y12, Y13).
minmaxoMinmaxo______minmaxo(Y10, succ(succ(zero)), Y12, Y13) :- gtoMinmaxo______minmaxo(Y10, Y12, Y13).
leo______minmaxo(succ(zero), zero).
leo______minmaxo(zero, succ(zero)).
minmaxo(succ(succ(zero)), succ(succ(succ(zero)))).
minmaxo(succ(succ(succ(zero))), succ(succ(zero))).
gtoMinmaxo______minmaxo(succ(zero), Y20, Y21) :- _minmaxo(Y20, Y21).
gtoMinmaxo______minmaxo(zero, Y20, Y21) :- _gtoMinmaxo(Y20, Y21).
_minmaxo(zero, succ(succ(succ(zero)))).
_minmaxo(succ(succ(succ(zero))), zero).
_gtoMinmaxo(succ(zero), succ(succ(succ(zero)))).
_gtoMinmaxo(succ(succ(succ(zero))), succ(zero)).
_minmaxoMinmaxo______minmaxo(Y27, succ(succ(succ(zero))), Y29, Y30) :- __gtoMinmaxo______minmaxo(Y27, Y29, Y30).
__leo______minmaxo(succ(zero), zero).
__leo______minmaxo(zero, succ(zero)).
__gtoMinmaxo______minmaxo(succ(zero), Y35, Y36) :- __________minmaxo(Y35, Y36).
__gtoMinmaxo______minmaxo(zero, Y35, Y36) :- ____minmaxo(Y35, Y36).
__________minmaxo(zero, succ(succ(zero))).
__________minmaxo(succ(succ(zero)), zero).
____minmaxo(succ(zero), succ(succ(zero))).
____minmaxo(succ(succ(zero)), succ(zero)).
_minmaxoMinmaxoMinmaxo__________minmaxo(Y42, Y43, Y44, Y45) :- __minmaxoMinmaxo__________minmaxo(Y42, Y43, Y44, Y45).
_minmaxoMinmaxoMinmaxo__________minmaxo(Y42, Y43, Y44, Y45) :- ___minmaxoMinmaxo__________minmaxo(Y42, Y43, Y44, Y45).
__minmaxoMinmaxo__________minmaxo(Y47, Y48, Y49, Y50) :- _leo__________minmaxo(Y47, Y48), __minmaxo(Y49, Y50).
__minmaxoMinmaxo__________minmaxo(Y47, succ(zero), Y49, Y50) :- _____gtoMinmaxo__________minmaxo(Y47, Y49, Y50).
_leo__________minmaxo(Y52, zero) :- __________minmaxo(Y52, zero).
_leo__________minmaxo(Y52, succ(zero)) :- __________minmaxo(Y52, succ(zero)).
__minmaxo(succ(zero), succ(succ(succ(zero)))).
__minmaxo(succ(succ(succ(zero))), succ(zero)).
_____gtoMinmaxo__________minmaxo(Y56, zero, succ(succ(succ(zero)))) :- __________minmaxo(Y56, zero).
_____gtoMinmaxo__________minmaxo(Y56, succ(succ(succ(zero))), zero) :- __________minmaxo(Y56, zero).
___minmaxoMinmaxo__________minmaxo(Y60, succ(succ(succ(zero))), Y62, Y63) :- ______gtoMinmaxo__________minmaxo(Y60, Y62, Y63).
__leo__________minmaxo(Y65, zero) :- __________minmaxo(Y65, zero).
__leo__________minmaxo(zero, succ(succ(zero))).
______gtoMinmaxo__________minmaxo(succ(succ(zero)), Y68, Y69) :- ______minmaxo(Y68, Y69).
______minmaxo(zero, succ(zero)).
______minmaxo(succ(zero), zero).
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y73, Y74, Y75, Y76) :- ____minmaxoMinmaxo_minmaxo(Y73, Y74, Y75, Y76).
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y73, Y74, Y75, Y76) :- _____minmaxoMinmaxo_minmaxo(Y73, Y74, Y75, Y76).
____minmaxoMinmaxo_minmaxo(Y77, Y78, Y79, Y80) :- _leo_minmaxo(Y77, Y78), ____minmaxo(Y79, Y80).
____minmaxoMinmaxo_minmaxo(Y77, succ(zero), Y79, Y80) :- _________gtoMinmaxo_minmaxo(Y77, Y79, Y80).
_leo_minmaxo(Y82, zero) :- _minmaxo(Y82, zero).
_leo_minmaxo(Y82, succ(zero)) :- _minmaxo(Y82, succ(zero)).
_________gtoMinmaxo_minmaxo(Y84, zero, succ(succ(zero))) :- _minmaxo(Y84, zero).
_________gtoMinmaxo_minmaxo(Y84, succ(succ(zero)), zero) :- _minmaxo(Y84, zero).
_____minmaxoMinmaxo_minmaxo(Y88, succ(succ(zero)), Y90, Y91) :- __________gtoMinmaxo_minmaxo(Y88, Y90, Y91).
leo_minmaxo(Y93, zero) :- _minmaxo(Y93, zero).
__________gtoMinmaxo_minmaxo(succ(succ(succ(zero))), Y96, Y97) :- ______minmaxo(Y96, Y97).