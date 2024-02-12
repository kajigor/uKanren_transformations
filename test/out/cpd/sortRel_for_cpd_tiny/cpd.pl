sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y2, Y5, Y8, Y9) :- minmaxoMinmaxoMinmaxo(Q1, Y5, Y8, Y9), ____minmaxo(Y2, Q1).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y2, Y5, Y8, Y9) :- _minmaxoMinmaxoMinmaxo(Q1, Y5, Y8, Y9), _______minmaxo(Y2, Q1).
minmaxoMinmaxoMinmaxo(Y11, Y12, Y15, Y16) :- minmaxoMinmaxo(Y11, Y12, Y15, Y16).
minmaxoMinmaxoMinmaxo(Y11, Y12, Y15, Y16) :- _minmaxoMinmaxo(Y11, Y12, Y15, Y16).
minmaxoMinmaxo(Y19, Y19, Y21, Y22) :- leo(Y19), minmaxo(Y21, Y22).
minmaxoMinmaxo(Y18, succ(zero), Y21, Y22) :- gtoMinmaxo(Y18, Y21, Y22).
leo(zero).
leo(succ(zero)).
minmaxo(succ(zero), succ(succ(zero))) :- leo(zero).
minmaxo(succ(succ(zero)), succ(zero)).
gtoMinmaxo(zero, Y27, Y28) :- _minmaxo(Y27, Y28).
_minmaxo(zero, succ(succ(zero))).
_minmaxo(succ(succ(zero)), zero).
_minmaxoMinmaxo(Y32, Y32, Y34, Y35) :- _leo(Y32), __minmaxo(Y34, Y35).
_minmaxoMinmaxo(Y31, succ(succ(zero)), Y34, Y35) :- _gtoMinmaxo(Y31, Y34, Y35).
_leo(zero).
_leo(succ(Q1)) :- leo(Q1).
__minmaxo(succ(succ(zero)), succ(zero)) :- leo(succ(succ(zero))).
_gtoMinmaxo(zero, Y40, Y41) :- _______minmaxo(Y40, Y41).
_gtoMinmaxo(succ(Q1), Y40, Y41) :- __gtoMinmaxo(Y40, Y41, Q1).
__gtoMinmaxo(Y42, Y43, zero) :- ___minmaxo(Y42, Y43).
___minmaxo(succ(zero), succ(zero)) :- leo(succ(zero)).
____minmaxo(zero, zero).
_minmaxoMinmaxoMinmaxo(Y49, Y50, Y53, Y54) :- __minmaxoMinmaxo(Y49, Y50, Y53, Y54).
_minmaxoMinmaxoMinmaxo(Y49, Y50, Y53, Y54) :- ___minmaxoMinmaxo(Y49, Y50, Y53, Y54).
__minmaxoMinmaxo(zero, zero, Y59, Y60) :- _minmaxo(Y59, Y60).
___minmaxoMinmaxo(Y62, Y62, Y64, Y65) :- _leo(Y62), _____minmaxo(Y64, Y65).
___minmaxoMinmaxo(Y61, succ(succ(zero)), Y64, Y65) :- ___gtoMinmaxo(Y61, Y64, Y65).
___gtoMinmaxo(zero, Y67, Y68) :- ____minmaxo(Y67, Y68).
___gtoMinmaxo(succ(Q1), Y67, Y68) :- ____gtoMinmaxo(Y67, Y68, Q1).
____gtoMinmaxo(Y69, Y70, zero) :- ______minmaxo(Y69, Y70).
_______minmaxo(zero, succ(zero)) :- leo(zero).
_______minmaxo(succ(zero), zero).
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y75, Y78, Y81, Y82) :- __minmaxoMinmaxoMinmaxo(Q1, Y78, Q2, Q3, Y81, Y82, Q4), ____minmaxo(Y75, Q1).
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y75, Y78, Y81, Y82) :- ___minmaxoMinmaxoMinmaxo(Q1, Y78, Y81, Y82), _minmaxo(Y75, Q1).
___minmaxoMinmaxoMinmaxo(Y84, Y85, Y88, Y89) :- ____minmaxoMinmaxo(Y84, Y85, Y88, Y89).
___minmaxoMinmaxoMinmaxo(Y84, Y85, Y88, Y89) :- _____minmaxoMinmaxo(Y84, Y85, Y88, Y89).
____minmaxoMinmaxo(zero, zero, Y94, Y95) :- _______minmaxo(Y94, Y95).
_____minmaxoMinmaxo(Y97, Y97, Y99, Y100) :- leo(Y97), ______minmaxo(Y99, Y100).
_____minmaxoMinmaxo(Y96, succ(zero), Y99, Y100) :- _____gtoMinmaxo(Y96, Y99, Y100).
_____gtoMinmaxo(zero, Y102, Y103) :- ____minmaxo(Y102, Y103).