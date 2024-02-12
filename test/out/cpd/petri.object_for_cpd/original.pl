fail() :- fail().
unsafe(X, P, s(s(X)), Y, C).
unsafe(s(X1), s(P1), Cs, Y, C) :- unsafe(X1, P1, s(Cs), Y, C).
unsafe(X, P, s(Cs1), Y, C) :- unsafe(X, s(P), Cs1, s(Y), C).
unsafe(X, P, Cs, s(Y1), C) :- unsafe(s(X), P, Cs, Y1, s(C)).