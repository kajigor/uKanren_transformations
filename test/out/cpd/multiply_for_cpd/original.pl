fail() :- fail().
add(zero, Z, Z).
add(succ(T1), Y, succ(T2)) :- add(T1, Y, T2).
multiply(X, zero, zero).
multiply(X, succ(T), Z) :- add(X, Z1, Z), multiply(X, T, Z1).