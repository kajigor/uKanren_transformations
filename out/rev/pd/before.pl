reverso(nil, nil) :- success().
reverso(cons(X2, X3), cons(X2, nil)) :- success(), success(), _reverso(X3, nil), success().
reverso(cons(X2, X3), cons(X5, X7)) :- success(), success(), _reverso(X3, cons(X5, X6)), success(), appendo(X7, X2, X6).
_reverso(nil, nil) :- success().
_reverso(cons(X2, X3), cons(X2, nil)) :- success(), success(), _reverso(X3, nil), success().
_reverso(cons(X2, X3), cons(X5, X7)) :- success(), success(), _reverso(X3, cons(X5, X6)), success(), appendo(X7, X2, X6).
appendo(cons(Y5, nil), Y5, nil).
appendo(cons(X5, X7), Y5, cons(X5, X6)) :- appendo(X7, Y5, X6).