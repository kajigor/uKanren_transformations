matcho(nil).
matcho(Y0) :- appendoAppendo(Y0).
appendoAppendo(Y1) :- appendo(Y1).
appendoAppendo(Y1) :- _appendoAppendo(Y1).
appendo(cons(s(s(o)), nil)).
appendo(nil).
_appendoAppendo(Y7) :- _appendo(Y7).
_appendoAppendo(Y7) :- __appendoAppendo(Y7).
_appendo(cons(s(s(o)), cons(s(o), nil))).
_appendo(Y11) :- __appendo(Y11).
__appendo(cons(s(o), nil)).
__appendo(nil).
__appendoAppendo(Y15) :- ___appendo(Y15).
__appendoAppendo(Y15) :- ___appendoAppendo(Y15).
___appendo(cons(s(s(o)), cons(s(o), cons(s(o), nil)))).
___appendo(Y19) :- ____appendo(Y19).
____appendo(cons(s(o), cons(s(o), nil))).
____appendo(Y21) :- __appendo(Y21).
___appendoAppendo(Y23) :- _____appendo(Y23).
___appendoAppendo(Y23) :- ____appendoAppendo(Y23).
_____appendo(cons(s(s(o)), cons(s(o), cons(s(o), cons(o, nil))))).
_____appendo(Y27) :- ______appendo(Y27).
______appendo(cons(s(o), cons(s(o), cons(o, nil)))).
______appendo(Y29) :- _______appendo(Y29).
_______appendo(cons(s(o), cons(o, nil))).
_______appendo(Y31) :- ________appendo(Y31).
________appendo(cons(o, nil)).
________appendo(nil).
____appendoAppendo(Y35) :- _________appendo(Y35).
____appendoAppendo(Y35) :- _____appendoAppendo(Y35).
_________appendo(cons(s(s(o)), cons(s(o), cons(s(o), cons(o, cons(o, nil)))))).
_________appendo(Y39) :- __________appendo(Y39).
__________appendo(cons(s(o), cons(s(o), cons(o, cons(o, nil))))).
__________appendo(Y41) :- ___________appendo(Y41).
___________appendo(cons(s(o), cons(o, cons(o, nil)))).
___________appendo(Y43) :- ____________appendo(Y43).
____________appendo(cons(o, cons(o, nil))).
____________appendo(Y45) :- ________appendo(Y45).
_____appendoAppendo(Y47) :- _____________appendo(Y47).
_____appendoAppendo(Y47) :- ______appendoAppendo(Y47).
_____________appendo(cons(s(s(o)), cons(s(o), cons(s(o), cons(o, cons(o, cons(s(s(o)), nil))))))).
_____________appendo(Y51) :- ______________appendo(Y51).
______________appendo(cons(s(o), cons(s(o), cons(o, cons(o, cons(s(s(o)), nil)))))).
______________appendo(Y53) :- _______________appendo(Y53).
_______________appendo(cons(s(o), cons(o, cons(o, cons(s(s(o)), nil))))).
_______________appendo(Y55) :- ________________appendo(Y55).
________________appendo(cons(o, cons(o, cons(s(s(o)), nil)))).
________________appendo(Y57) :- _________________appendo(Y57).
_________________appendo(cons(o, cons(s(s(o)), nil))).
_________________appendo(Y59) :- appendo(Y59).
______appendoAppendo(Y61) :- __________________appendo(Y61).
______appendoAppendo(Y61) :- ________________________appendo(Y61).
__________________appendo(cons(s(s(o)), cons(s(o), cons(s(o), cons(o, cons(o, cons(s(s(o)), cons(o, nil)))))))).
__________________appendo(Y65) :- ___________________appendo(Y65).
___________________appendo(cons(s(o), cons(s(o), cons(o, cons(o, cons(s(s(o)), cons(o, nil))))))).
___________________appendo(Y67) :- ____________________appendo(Y67).
____________________appendo(cons(s(o), cons(o, cons(o, cons(s(s(o)), cons(o, nil)))))).
____________________appendo(Y69) :- _____________________appendo(Y69).
_____________________appendo(cons(o, cons(o, cons(s(s(o)), cons(o, nil))))).
_____________________appendo(Y71) :- ______________________appendo(Y71).
______________________appendo(cons(o, cons(s(s(o)), cons(o, nil)))).
______________________appendo(Y73) :- _______________________appendo(Y73).
_______________________appendo(cons(s(s(o)), cons(o, nil))).
_______________________appendo(Y75) :- ________appendo(Y75).
________________________appendo(cons(s(s(o)), cons(s(o), cons(s(o), cons(o, cons(o, cons(s(s(o)), cons(o, cons(s(o), nil))))))))).
________________________appendo(Y77) :- _________________________appendo(Y77).
_________________________appendo(cons(s(o), cons(s(o), cons(o, cons(o, cons(s(s(o)), cons(o, cons(s(o), nil)))))))).
_________________________appendo(Y79) :- __________________________appendo(Y79).
__________________________appendo(cons(s(o), cons(o, cons(o, cons(s(s(o)), cons(o, cons(s(o), nil))))))).
__________________________appendo(Y81) :- ___________________________appendo(Y81).
___________________________appendo(cons(o, cons(o, cons(s(s(o)), cons(o, cons(s(o), nil)))))).
___________________________appendo(Y83) :- ____________________________appendo(Y83).
____________________________appendo(cons(o, cons(s(s(o)), cons(o, cons(s(o), nil))))).
____________________________appendo(Y85) :- _____________________________appendo(Y85).
_____________________________appendo(cons(s(s(o)), cons(o, cons(s(o), nil)))).
_____________________________appendo(Y87) :- ______________________________appendo(Y87).
______________________________appendo(cons(o, cons(s(o), nil))).
______________________________appendo(Y89) :- __appendo(Y89).