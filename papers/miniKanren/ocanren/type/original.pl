type(bConst(X), Gamma, boolean).
type(iConst(Y), Gamma, integer).
type(var(V), Gamma, Ttype) :- elemo(V, Gamma, Ttype).
type(abs(V, Vt, T), Gamma, Ttype) :- type(T, cons(pair(V, Vt), Gamma), Ttype).
type(app(T1, T2), Gamma, Ttype) :- type(T1, Gamma, arrow(T1type, T2type)), type(T2, Gamma, T2type).
elemo(o, cons(H, T), H).
elemo(s(N1), cons(H, T), V) :- elemo(N1, T, V).