double_appendo(nil, Y1, Y2) :- appendo(Y2, Y1).
double_appendo(cons(s(o), Q1), Y1, Y2) :- appendoAppendo(Y1, Y2, Q1).
appendo(cons(s(o), cons(s(s(o)), cons(s(s(s(o))), cons(o, cons(o, cons(s(o), cons(s(s(o)), nil))))))), nil).
appendo(Y3, cons(s(o), Q1)) :- _appendo(Y3, Q1).
_appendo(cons(s(s(o)), cons(s(s(s(o))), cons(o, cons(o, cons(s(o), cons(s(s(o)), nil)))))), nil).
_appendo(Y5, cons(s(s(o)), Q1)) :- __appendo(Y5, Q1).
__appendo(cons(s(s(s(o))), cons(o, cons(o, cons(s(o), cons(s(s(o)), nil))))), nil).
__appendo(Y7, cons(s(s(s(o))), Q1)) :- ___appendo(Y7, Q1).
___appendo(cons(o, cons(o, cons(s(o), cons(s(s(o)), nil)))), nil).
___appendo(Y9, cons(o, Q1)) :- ____appendo(Y9, Q1).
____appendo(cons(o, cons(s(o), cons(s(s(o)), nil))), nil).
____appendo(Y11, cons(o, Q1)) :- _____appendo(Y11, Q1).
_____appendo(cons(s(o), cons(s(s(o)), nil)), nil).
_____appendo(Y13, cons(s(o), Q1)) :- ______appendo(Y13, Q1).
______appendo(cons(s(s(o)), nil), nil).
______appendo(nil, cons(s(s(o)), nil)).
appendoAppendo(Y17, Y18, nil) :- _appendo(Y18, Y17).
appendoAppendo(Y17, Y18, cons(s(s(o)), Q1)) :- _appendoAppendo(Y17, Y18, Q1).
_appendoAppendo(Y21, Y22, nil) :- __appendo(Y22, Y21).
_appendoAppendo(Y21, Y22, cons(s(s(s(o))), Q1)) :- __appendoAppendo(Y21, Y22, Q1).
__appendoAppendo(Y25, Y26, nil) :- ___appendo(Y26, Y25).
__appendoAppendo(Y25, Y26, cons(o, Q1)) :- ___appendoAppendo(Y25, Y26, Q1).
___appendoAppendo(Y29, Y30, nil) :- ____appendo(Y30, Y29).
___appendoAppendo(Y29, Y30, cons(o, Q1)) :- ____appendoAppendo(Y29, Y30, Q1).
____appendoAppendo(Y33, Y34, nil) :- _____appendo(Y34, Y33).
____appendoAppendo(Y33, Y34, cons(s(o), Q1)) :- _____appendoAppendo(Y33, Y34, Q1).
_____appendoAppendo(Y37, Y38, nil) :- ______appendo(Y38, Y37).
_____appendoAppendo(Y37, nil, cons(s(s(o)), Q1)) :- _______appendo(Y37, Q1).
_______appendo(nil, nil).