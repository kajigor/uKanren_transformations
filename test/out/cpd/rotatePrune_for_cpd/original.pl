fail() :- fail().
rp(T1, T2) :- prune(U, T2), rotate(T1, U).
rotate(leaf(N), leaf(N)).
rotate(tree(L, N, R), tree(Rl, N, Rr)) :- rotate(L, Rl), rotate(R, Rr).
rotate(tree(L, N, R), tree(Rr, N, Rl)) :- rotate(L, Rl), rotate(R, Rr).
prune(leaf(N), leaf(N)).
prune(tree(L, o, R), leaf(o)).
prune(tree(L, s(N), R), tree(Pl, s(N), Pr)) :- prune(L, Pl), prune(R, Pr).