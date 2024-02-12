solve(Y0, Y1, Y2) :- solve_atomSolve(Y0, Y1, Y2).
solve_atom(Y3, Y4, Y5) :- __solve(Y3, Y4, Y5).
solve_atom(cons(Q1, Q3), Y4, cons(Q1, Q2)) :- my_clauseSolve(Q3, Y4, Q2).
solve_atom(nil, Y5, Y5).
_solve(Y6, Y7) :- _solve_atom(Y6, Y7).
__solve(Y8, Y9, Y10) :- my_clauseSolve(Y8, Y9, Y10).
my_clauseSolve(Y11, Y12, Y13) :- __solve(Y11, Y12, Y13).
my_clauseSolve(cons(Q1, Q3), Y12, cons(Q1, Q2)) :- my_clauseSolve(Q3, Y12, Q2).
my_clauseSolve(nil, Y13, Y13).
_solve_atom(cons(s, cons(s(o), cons(s(s(o)), cons(s(o), nil)))), nil).
_solve_atom(Y15, cons(s, Q1)) :- __solve_atom(Q1, Y15).
_solve_atom(Y15, Y16) :- _solve_atom(Y15, Y16).
__solve_atom(nil, cons(s(o), cons(s(s(o)), cons(s(o), nil)))).
__solve_atom(cons(s(o), Q1), Y18) :- ___solve_atom(Q1, Y18).
__solve_atom(Y17, Y18) :- __solve_atom(Y17, Y18).
___solve_atom(nil, cons(s(s(o)), cons(s(o), nil))).
___solve_atom(cons(s(s(o)), Q1), Y20) :- ____solve_atom(Q1, Y20).
___solve_atom(Y19, Y20) :- ___solve_atom(Y19, Y20).
____solve_atom(nil, cons(s(o), nil)).
____solve_atom(cons(s(o), Q1), Y22) :- _____solve_atom(Q1, Y22).
____solve_atom(Y21, Y22) :- ____solve_atom(Y21, Y22).
_____solve_atom(nil, nil).
_____solve_atom(Y23, Y24) :- _____solve_atom(Y23, Y24).
solve_atomSolve(Y25, Y26, Y27) :- solve_atom(Y25, Y26, Q1), _solve(Y27, Q1).
solve_atomSolve(Y25, Y26, Y27) :- solve_atomSolve(Y25, Y26, Y27).