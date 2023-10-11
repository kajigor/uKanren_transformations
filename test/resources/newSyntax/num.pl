fail() :- fail().
notZero(s(Y)).
addo(o, Z, Z).
addo(s(X_0), Y, s(Z_0)) :- addo(X_0, Y, Z_0).
mulo(o, Y, o).
mulo(s(X_0), Y, Z) :- addo(Y, Z_0, Z), mulo(X_0, Y, Z_0).
leo(o, Y, trueo).
leo(s(Zz), o, falso).
leo(s(X_0), s(Y_0), B) :- leo(X_0, Y_0, B).
gto(s(Zz), o, trueo).
gto(o, Y, falso).
gto(s(X_0), s(Y_0), B) :- gto(X_0, Y_0, B).
geo(X, Y, Z) :- leo(Y, X, Z).
lto(X, Y, Z) :- gto(Y, X, Z).