doubleAppendo(Y0, Y1, Y2, Y3) :- appendo(Y0, Y1, Q1), appendo(Q1, Y2, Y3).
appendo(nil, Y6, Y6).
appendo(cons(Q1, Q2), Y5, cons(Q1, Q3)) :- appendo(Q2, Y5, Q3).