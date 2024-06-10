fail() :- fail().
add(o, Z, Z).
add(s(T1), Y, s(T2)) :- add(T1, Y, T2).
multiply(X, o, o).
multiply(X, s(T), Z) :- add(X, Z1, Z), multiply(X, T, Z1).
le(o, N).
le(s(T), s(T1)) :- le(T, T1).
square(Ns, Son) :- multiply(Ns, Ns, Son).
sum(Ns, S) :- sum1(Ns, o, S).
sumtrsquaretr(Xt, S) :- squaretr(Xt, Soxt), sumtr(Soxt, S).
sumtr(leaf(S), S).
sumtr(branch(Xt, Yt), S) :- sumtr(Xt, Sx), sumtr(Yt, Sy), add(Sx, Sy, S).
squaretr(leaf(X), leaf(Sox)) :- square(X, Sox).
squaretr(branch(Xt, Yt), branch(Soxt, Soyt)) :- squaretr(Xt, Soxt), squaretr(Yt, Soyt).