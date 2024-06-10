fail() :- fail().
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y1, Y2, Y3, Y4) :- minmaxoMinmaxoMinmaxo______minmaxo(Y1, Y2, Y3, Y4).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y1, Y2, Y3, Y4) :- _minmaxoMinmaxoMinmaxo__________minmaxo(Y1, Y2, Y3, Y4).
minmaxoMinmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8) :- minmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8).
minmaxoMinmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8) :- _minmaxoMinmaxo______minmaxo(Y5, Y6, Y7, Y8).
minmaxoMinmaxo______minmaxo(Y9, Y10, Y11, Y12) :- leo______minmaxo(Y9, Y10), minmaxo(Y11, Y12).
minmaxoMinmaxo______minmaxo(Y9, succ(succ(zero)), Y11, Y12) :- gtoMinmaxo______minmaxo(Y9, Y11, Y12).
leo______minmaxo(succ(zero), zero).
leo______minmaxo(zero, succ(zero)).
minmaxo(succ(succ(zero)), succ(succ(succ(zero)))).
minmaxo(succ(succ(succ(zero))), succ(succ(zero))).
gtoMinmaxo______minmaxo(succ(zero), Y18, Y19) :- _minmaxo(Y18, Y19).
gtoMinmaxo______minmaxo(zero, Y18, Y19) :- _gtoMinmaxo(Y18, Y19).
_minmaxo(zero, succ(succ(succ(zero)))).
_minmaxo(succ(succ(succ(zero))), zero).
_gtoMinmaxo(succ(zero), succ(succ(succ(zero)))).
_gtoMinmaxo(succ(succ(succ(zero))), succ(zero)).
_minmaxoMinmaxo______minmaxo(succ(zero), succ(succ(succ(zero))), Y26, Y27) :- __________minmaxo(Y26, Y27).
_minmaxoMinmaxo______minmaxo(zero, succ(succ(succ(zero))), Y26, Y27) :- ____minmaxo(Y26, Y27).
__________minmaxo(zero, succ(succ(zero))).
__________minmaxo(succ(succ(zero)), zero).
____minmaxo(succ(zero), succ(succ(zero))).
____minmaxo(succ(succ(zero)), succ(zero)).
_minmaxoMinmaxoMinmaxo__________minmaxo(Y32, Y33, Y34, Y35) :- __minmaxoMinmaxo__________minmaxo(Y32, Y33, Y34, Y35).
_minmaxoMinmaxoMinmaxo__________minmaxo(Y32, Y33, Y34, Y35) :- ___minmaxoMinmaxo__________minmaxo(Y32, Y33, Y34, Y35).
__minmaxoMinmaxo__________minmaxo(Y36, Y37, Y38, Y39) :- _leo__________minmaxo(Y36, Y37), __minmaxo(Y38, Y39).
__minmaxoMinmaxo__________minmaxo(Y36, succ(zero), Y38, Y39) :- _____gtoMinmaxo__________minmaxo(Y36, Y38, Y39).
_leo__________minmaxo(Y40, zero) :- __________minmaxo(Y40, zero).
_leo__________minmaxo(Y40, succ(zero)) :- __________minmaxo(Y40, succ(zero)).
__minmaxo(succ(zero), succ(succ(succ(zero)))).
__minmaxo(succ(succ(succ(zero))), succ(zero)).
_____gtoMinmaxo__________minmaxo(Y44, zero, succ(succ(succ(zero)))) :- __________minmaxo(Y44, zero).
_____gtoMinmaxo__________minmaxo(Y44, succ(succ(succ(zero))), zero) :- __________minmaxo(Y44, zero).
___minmaxoMinmaxo__________minmaxo(succ(succ(zero)), succ(succ(succ(zero))), zero, succ(zero)).
___minmaxoMinmaxo__________minmaxo(succ(succ(zero)), succ(succ(succ(zero))), succ(zero), zero).
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y51, Y52, Y53, Y54) :- ____minmaxoMinmaxo_minmaxo(Y51, Y52, Y53, Y54).
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y51, Y52, Y53, Y54) :- _____minmaxoMinmaxo_minmaxo(Y51, Y52, Y53, Y54).
____minmaxoMinmaxo_minmaxo(Y55, Y56, Y57, Y58) :- _leo_minmaxo(Y55, Y56), ____minmaxo(Y57, Y58).
____minmaxoMinmaxo_minmaxo(Y55, succ(zero), Y57, Y58) :- _________gtoMinmaxo_minmaxo(Y55, Y57, Y58).
_leo_minmaxo(Y59, zero) :- _minmaxo(Y59, zero).
_leo_minmaxo(Y59, succ(zero)) :- _minmaxo(Y59, succ(zero)).
_________gtoMinmaxo_minmaxo(Y61, zero, succ(succ(zero))) :- _minmaxo(Y61, zero).
_________gtoMinmaxo_minmaxo(Y61, succ(succ(zero)), zero) :- _minmaxo(Y61, zero).
_____minmaxoMinmaxo_minmaxo(succ(succ(succ(zero))), succ(succ(zero)), zero, succ(zero)).
_____minmaxoMinmaxo_minmaxo(succ(succ(succ(zero))), succ(succ(zero)), succ(zero), zero).