doubleAppendo(nil, X4, Y2, Y3) :- success(), success(), success(), success(), appendo(X4, Y2, Y3).
doubleAppendo(cons(X5, X6), Y1, Y2, Y3) :- success(), success(), success(), appendo(X6, Y1, X7), success(), appendo(cons(X5, X7), Y2, Y3).
appendo(nil, Y6, Y6) :- success().
appendo(cons(X5, X6), Y5, cons(X5, X7)) :- success(), appendo(X6, Y5, X7).