fail() :- fail().
solve(nil).
solve(cons(A, T)) :- solve_atom(A), solve(T).
solve_atom(A) :- my_clause(A, B), solve(B).
my_clause(doubleApp(X, Y, Z, R), cons(app(X, Y, I), cons(app(I, Z, R), nil))).
my_clause(app(nil, L, L), nil).
my_clause(app(cons(H, X), Y, cons(H, Z)), cons(app(X, Y, Z), nil)).
my_clause(Clause, cons(Clause, nil)).
my_clause(solve2(nil), nil).
my_clause(solve2(cons(A, T)), cons(solve_atom2(A), cons(solve2(T), nil))).
my_clause(solve_atom2(A), cons(my_clause2(A, B), cons(solve2(B), nil))).
my_clause(my_clause2(app(nil, L, L), nil), nil).
my_clause(my_clause2(app(cons(H, X), Y, cons(H, Z)), cons(app(X, Y, Z), nil)), nil).
test2(R) :- solve_atom(solve_atom2(app(cons(o, cons(s(o), cons(s(s(o)), nil))), cons(o, cons(s(o), cons(s(s(o)), cons(s(s(s(o))), nil)))), R))).
test1(R) :- solve_atom(app(cons(o, cons(s(o), cons(s(s(o)), nil))), cons(o, cons(s(o), cons(s(s(o)), cons(s(s(s(o))), nil)))), R)).