typecheck_(Gamma, iConst_(Q1), some(integer)).
typecheck_(Gamma, bConst_(Q3), some(boolean)).
typecheck_(Gamma, var_(V), Q0) :- nthOpt(Gamma, V, Q0).
typecheck_(Gamma, plus_(X, Y), none) :- typecheck_(Gamma, X, none).
typecheck_(Gamma, plus_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, none).
typecheck_(Gamma, plus_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, false), typeEq(Y1, integer, Q16).
typecheck_(Gamma, plus_(X, Y), some(integer)) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, true), typeEq(Y1, integer, true).
typecheck_(Gamma, plus_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, true), typeEq(Y1, integer, false).
typecheck_(Gamma, mult_(X, Y), none) :- typecheck_(Gamma, X, none).
typecheck_(Gamma, mult_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, none).
typecheck_(Gamma, mult_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, false), typeEq(Y1, integer, Q32).
typecheck_(Gamma, mult_(X, Y), some(integer)) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, true), typeEq(Y1, integer, true).
typecheck_(Gamma, mult_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, true), typeEq(Y1, integer, false).
typecheck_(Gamma, equal_(X, Y), none) :- typecheck_(Gamma, X, none).
typecheck_(Gamma, equal_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, none).
typecheck_(Gamma, equal_(X, Y), some(boolean)) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, Y1, true).
typecheck_(Gamma, equal_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, Y1, false).
typecheck_(Gamma, less_(X, Y), none) :- typecheck_(Gamma, X, none).
typecheck_(Gamma, less_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, none).
typecheck_(Gamma, less_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, false), typeEq(Y1, integer, Q58).
typecheck_(Gamma, less_(X, Y), some(boolean)) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, true), typeEq(Y1, integer, true).
typecheck_(Gamma, less_(X, Y), none) :- typecheck_(Gamma, X, some(X1)), typecheck_(Gamma, Y, some(Y1)), typeEq(X1, integer, true), typeEq(Y1, integer, false).
typecheck_(Gamma, if_(C, T, E), none) :- typecheck_(Gamma, C, none).
typecheck_(Gamma, if_(C, T, E), none) :- typecheck_(Gamma, C, some(C1)), typeEq(C1, boolean, true), typecheck_(Gamma, T, none).
typecheck_(Gamma, if_(C, T, E), none) :- typecheck_(Gamma, C, some(C1)), typeEq(C1, boolean, true), typecheck_(Gamma, T, some(T1)), typecheck_(Gamma, E, none).
typecheck_(Gamma, if_(C, T, E), some(T1)) :- typecheck_(Gamma, C, some(C1)), typeEq(C1, boolean, true), typecheck_(Gamma, T, some(T1)), typecheck_(Gamma, E, some(E1)), typeEq(T1, E1, true).
typecheck_(Gamma, if_(C, T, E), none) :- typecheck_(Gamma, C, some(C1)), typeEq(C1, boolean, true), typecheck_(Gamma, T, some(T1)), typecheck_(Gamma, E, some(E1)), typeEq(T1, E1, false).
typecheck_(Gamma, if_(C, T, E), none) :- typecheck_(Gamma, C, some(C1)), typeEq(C1, boolean, false).
typecheck_(Gamma, let_(V, B), none) :- typecheck_(Gamma, V, none).
typecheck_(Gamma, let_(V, B), Q0) :- typecheck_(Gamma, V, some(V1)), typecheck_(cons(V1, Gamma), B, Q0).
nthOpt(nil, N, none).
nthOpt(cons(H, T), o, some(H)).
nthOpt(cons(H, T), s(X), R) :- nthOpt(T, s(X), R).
typeEq(integer, integer, true).
typeEq(boolean, boolean, true).
typeEq(integer, boolean, false).
typeEq(boolean, integer, false).