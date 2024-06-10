solve(Y0, Y1, Y2) :- solve_atomSolve_atom(Y0, Y1, Y2).
solve(Y0, Y1, Y2) :- solve(Y0, Y1, Y2).
solve_atomSolve_atom(nil, Y4, Y5) :- solve_atom(Y4, Y5, cons(s, cons(s(o), cons(s(s(o)), cons(s(o), nil))))).
solve_atomSolve_atom(cons(Q1, Q2), Y4, Y5) :- solve_atom(Q2, Y4, Q3), solve_atom(cons(Q1, Q3), Y5, cons(s, cons(s(o), cons(s(s(o)), cons(s(o), nil))))).
solve_atomSolve_atom(Y3, Y4, Y5) :- solve_atomSolve_atom(Y3, Y4, Y5).
solve_atom(nil, Y9, Y9).
solve_atom(cons(Q1, Q3), Y8, cons(Q1, Q2)) :- solve_atom(Q3, Y8, Q2).
solve_atom(Y7, Y8, Y9) :- solve_atom(Y7, Y8, Y9).