doubleAppendo(nil, Q1, Y2, Y3) :- appendo(Q1, Y2, Y3).
doubleAppendo(cons(Q2, Q4), Y1, Y2, Y3) :- appendo(Q4, Y1, Q3), appendo(cons(Q2, Q3), Y2, Y3).
appendo(nil, Y6, Y6).
appendo(cons(Q1, Q3), Y5, cons(Q1, Q2)) :- appendo(Q3, Y5, Q2).