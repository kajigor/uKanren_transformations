doubleAppendo([H|T],Y,Z,[H|R]) :- doubleAppendo(T,Y,Z,R).
doubleAppendo([], Y, Z, R) :- appendo(Y,Z,R).

appendo([H|T], Y, [H|R]) :- appendo(T, Y, R).
appendo([], Y, Y).
