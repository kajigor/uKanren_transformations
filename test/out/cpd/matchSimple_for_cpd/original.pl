fail() :- fail().
match(Pat, T) :- match1(Pat, T, Pat, T).
neq(succ(T), zero).
neq(zero, succ(T)).
neq(succ(T), succ(T1)) :- neq(T, T1).
match1(nil, Tl, Pat, T).
match1(cons(B, Ps), cons(B, Ts), Pat, T) :- match1(Ps, Ts, Pat, T).
match1(cons(A, Ps), cons(B, Ts), Pat, cons(X, T1)) :- neq(A, B), match1(Pat, T1, Pat, T1).