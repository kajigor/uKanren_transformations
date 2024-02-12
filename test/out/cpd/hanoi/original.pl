fail() :- fail().
notEqStick(one, one, falso).
notEqStick(one, two, trueo).
notEqStick(one, thr, trueo).
notEqStick(two, one, trueo).
notEqStick(two, two, falso).
notEqStick(two, thr, trueo).
notEqStick(thr, one, trueo).
notEqStick(thr, two, trueo).
notEqStick(thr, thr, falso).
isNil(nil, trueo).
isNil(cons(Q41, Q42), falso).
less(o, s(B_0), trueo).
less(s(A_0), s(B_0), Q36) :- less(A_0, B_0, Q36).
get(one, triple(Q31, S2, S3), Q31).
get(two, triple(S1, Q31, S3), Q31).
get(thr, triple(S1, S2, Q31), Q31).
set(one, Stack, triple(S1, S2, S3), triple(Stack, S2, S3)).
set(two, Stack, triple(S1, S2, S3), triple(S1, Stack, S3)).
set(thr, Stack, triple(S1, S2, S3), triple(S1, S2, Stack)).
one_step(pair(FromN, ToN), State, Q13) :- notEqStick(FromN, ToN, trueo), get(FromN, State, cons(X, Xs)), get(ToN, State, nil), set(FromN, Xs, State, Q20), set(ToN, cons(X, nil), Q20, Q13).
one_step(pair(FromN, ToN), State, Q13) :- notEqStick(FromN, ToN, trueo), get(FromN, State, cons(X, Xs)), get(ToN, State, cons(Y, Ys)), less(X, Y, trueo), set(FromN, Xs, State, Q24), set(ToN, cons(X, cons(Y, Ys)), Q24, Q13).
check(State, nil, falso) :- get(one, State, Q7), isNil(Q7, falso), get(two, State, Q9), isNil(Q9, Q2).
check(State, nil, Q2) :- get(one, State, Q7), isNil(Q7, trueo), get(two, State, Q9), isNil(Q9, Q2).
check(State, cons(X, Xs), Q0) :- one_step(X, State, Q11), check(Q11, Xs, Q0).