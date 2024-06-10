fail() :- fail().
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y2, Y5, Y8, Y9) :- minmaxoMinmaxoMinmaxo(Q1, Y5, Y8, Y9), ______minmaxo(Y2, Q1).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y2, Y5, Y8, Y9) :- _minmaxoMinmaxoMinmaxo(Q1, Y5, Y8, Y9), __________minmaxo(Y2, Q1).
minmaxoMinmaxoMinmaxo(Y11, Y12, Y15, Y16) :- minmaxoMinmaxo(Y11, Y12, Y15, Y16).
minmaxoMinmaxoMinmaxo(Y11, Y12, Y15, Y16) :- _minmaxoMinmaxo(Y11, Y12, Y15, Y16).
minmaxoMinmaxo(Y19, Y19, Y21, Y22) :- leo(Y19), minmaxo(Y21, Y22).
minmaxoMinmaxo(Y18, succ(succ(zero)), Y21, Y22) :- gtoMinmaxo(Y18, Y21, Y22).
leo(zero).
leo(succ(Q1)) :- _leo(Q1).
_leo(zero).
_leo(succ(zero)).
minmaxo(succ(succ(zero)), succ(succ(succ(zero)))) :- leo(succ(zero)).
minmaxo(succ(succ(succ(zero))), succ(succ(zero))).
gtoMinmaxo(zero, Y28, Y29) :- _minmaxo(Y28, Y29).
gtoMinmaxo(succ(Q1), Y28, Y29) :- _gtoMinmaxo(Y28, Y29, Q1).
_minmaxo(zero, succ(succ(succ(zero)))).
_minmaxo(succ(succ(succ(zero))), zero).
_gtoMinmaxo(Y32, Y33, zero) :- __minmaxo(Y32, Y33).
__minmaxo(succ(zero), succ(succ(succ(zero)))) :- leo(zero).
__minmaxo(succ(succ(succ(zero))), succ(zero)).
_minmaxoMinmaxo(Y38, Y38, Y40, Y41) :- __leo(Y38), ___minmaxo(Y40, Y41).
_minmaxoMinmaxo(Y37, succ(succ(succ(zero))), Y40, Y41) :- __gtoMinmaxo(Y37, Y40, Y41).
__leo(zero).
__leo(succ(Q1)) :- leo(Q1).
___minmaxo(succ(succ(succ(zero))), succ(succ(zero))) :- leo(succ(succ(succ(zero)))).
__gtoMinmaxo(zero, Y46, Y47) :- __________minmaxo(Y46, Y47).
__gtoMinmaxo(succ(Q1), Y46, Y47) :- ___gtoMinmaxo(Y46, Y47, Q1).
___gtoMinmaxo(Y48, Y49, zero) :- ____minmaxo(Y48, Y49).
___gtoMinmaxo(Y48, Y49, succ(Q1)) :- ____gtoMinmaxo(Y48, Y49, Q1).
____minmaxo(succ(zero), succ(succ(zero))) :- leo(succ(zero)).
____minmaxo(succ(succ(zero)), succ(zero)).
____gtoMinmaxo(Y53, Y54, zero) :- _____minmaxo(Y53, Y54).
_____minmaxo(succ(succ(zero)), succ(succ(zero))) :- leo(succ(succ(zero))).
______minmaxo(zero, succ(zero)) :- _leo(zero).
______minmaxo(succ(zero), zero).
_minmaxoMinmaxoMinmaxo(Y60, Y61, Y64, Y65) :- __minmaxoMinmaxo(Y60, Y61, Y64, Y65).
_minmaxoMinmaxoMinmaxo(Y60, Y61, Y64, Y65) :- ___minmaxoMinmaxo(Y60, Y61, Y64, Y65).
__minmaxoMinmaxo(Y68, Y68, Y70, Y71) :- _leo(Y68), __minmaxo(Y70, Y71).
__minmaxoMinmaxo(Y67, succ(zero), Y70, Y71) :- _____gtoMinmaxo(Y67, Y70, Y71).
_____gtoMinmaxo(zero, Y73, Y74) :- _minmaxo(Y73, Y74).
___minmaxoMinmaxo(Y76, Y76, Y78, Y79) :- __leo(Y76), _______minmaxo(Y78, Y79).
___minmaxoMinmaxo(Y75, succ(succ(succ(zero))), Y78, Y79) :- ______gtoMinmaxo(Y75, Y78, Y79).
_______minmaxo(succ(succ(succ(zero))), succ(zero)) :- _leo(succ(succ(succ(zero)))).
______gtoMinmaxo(zero, Y83, Y84) :- ______minmaxo(Y83, Y84).
______gtoMinmaxo(succ(Q1), Y83, Y84) :- _______gtoMinmaxo(Y83, Y84, Q1).
_______gtoMinmaxo(Y85, Y86, zero) :- ________minmaxo(Y85, Y86).
_______gtoMinmaxo(Y85, Y86, succ(Q1)) :- ________gtoMinmaxo(Y85, Y86, Q1).
________minmaxo(succ(zero), succ(zero)) :- _leo(succ(zero)).
________gtoMinmaxo(Y90, Y91, zero) :- _________minmaxo(Y90, Y91).
_________minmaxo(succ(succ(zero)), succ(zero)) :- _leo(succ(succ(zero))).
__________minmaxo(zero, succ(succ(zero))) :- leo(zero).
__________minmaxo(succ(succ(zero)), zero).
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y98, Y101, Y104, Y105) :- ___minmaxoMinmaxoMinmaxo(Q1, Y101, Y104, Y105), _minmaxo(Y98, Q1).
___minmaxoMinmaxoMinmaxo(Y107, Y108, Y111, Y112) :- ____minmaxoMinmaxo(Y107, Y108, Y111, Y112).
___minmaxoMinmaxoMinmaxo(Y107, Y108, Y111, Y112) :- _____minmaxoMinmaxo(Y107, Y108, Y111, Y112).
____minmaxoMinmaxo(Y115, Y115, Y117, Y118) :- _leo(Y115), ____minmaxo(Y117, Y118).
____minmaxoMinmaxo(Y114, succ(zero), Y117, Y118) :- _________gtoMinmaxo(Y114, Y117, Y118).
_________gtoMinmaxo(zero, Y120, Y121) :- __________minmaxo(Y120, Y121).
_____minmaxoMinmaxo(Y123, Y123, Y125, Y126) :- leo(Y123), _________minmaxo(Y125, Y126).
_____minmaxoMinmaxo(Y122, succ(succ(zero)), Y125, Y126) :- __________gtoMinmaxo(Y122, Y125, Y126).
__________gtoMinmaxo(zero, Y128, Y129) :- ______minmaxo(Y128, Y129).
__________gtoMinmaxo(succ(Q1), Y128, Y129) :- ___________gtoMinmaxo(Y128, Y129, Q1).
___________gtoMinmaxo(Y130, Y131, zero) :- ________minmaxo(Y130, Y131).