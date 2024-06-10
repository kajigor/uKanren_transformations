fail() :- fail().
rr(X, Y) :- f(X, T), f(T, Y).
f(nil, nil).
f(cons(A, T), Y) :- h(A, T, Y).
h(A, nil, cons(A, nil)).
h(A, cons(B, S), Y) :- g(A, B, cons(B, S), S, Y).
neq(zero, succ(T)).
neq(succ(T), zero).
neq(succ(T), succ(T1)) :- neq(T, T1).
g(B, B, T, S, cons(B, Y1)) :- f(S, Y1).
g(A1, B, T, S, cons(A1, Y1)) :- neq(A1, B), f(T, Y1).