fail() :- fail().
solve1(nil, Res, Res).
solve1(cons(Head, Tail), Depth, Res) :- claus(Head, Body), solve1(Body, s(Depth), IntDepth), solve1(Tail, IntDepth, Res).
claus(member(X, cons(X, T)), nil).
claus(member(X, cons(Y, T)), cons(member(X, T), nil)).
claus(inBoth(X, L1, L2), cons(member(X, L1), cons(member(X, L2), nil))).
claus(app(nil, L, L), nil).
claus(app(cons(H, X), Y, cons(H, Z)), cons(app(X, Y, Z), nil)).
claus(delete(X, cons(X, T), T), nil).
claus(delete(X, cons(Y, T), cons(Y, D)), cons(delete(X, T, D), nil)).
claus(test(A, L1, L2, Res), cons(inBoth(A, L1, L2), cons(delete(A, L1, D1), cons(app(D1, L2, Res), nil)))).
solve(Lst, Res) :- solve1(Lst, o, Res).