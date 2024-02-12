fail() :- fail().
solve(pair(G1, G2), nil) :- solve(G1, nil), solve(G2, nil).
solve(From, Goal) :- prove(From, Goal).
prove(G, A) :- member(G, A).
prove(G, A) :- neg(G, Gn), contrapositive(impl(Gn, B)), proveall(B, cons(Gn, A)).
proveall(nil, G).
proveall(cons(H, T), G) :- prove(H, A), proveall(T, A).
contrapositive(impl(G, B)) :- input_clause(cons(G, B)).
contrapositive(impl(G, cons(B, Bs1))) :- input_clause(cons(B, Bs)), contrapositive1(G, Bs, Bs1).
contrapositive1(G, cons(G, Xs), Xs).
contrapositive1(G, cons(X, T1), cons(X, T2)) :- contrapositive1(G, T1, T2).
member(X, cons(X, T)).
member(X, cons(H, T)) :- member(X, T).
neg(neg(F), pos(F)).
neg(pos(F), neg(F)).
input_clause(cons(pos(app(nil, L, L)), nil)).
input_clause(cons(pos(app(cons(H, X), Y, cons(H, Z))), cons(neg(app(X, Y, Z)), nil))).