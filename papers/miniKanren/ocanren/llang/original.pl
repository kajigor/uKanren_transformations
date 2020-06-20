type(bConst(X), Gamma, boolean).
type(iConst(Y), Gamma, integer).
type(var_(V), Gamma, Ttype) :- assoco(V, Gamma, Ttype).
type(plus(M, N), Gamma, integer) :- type(M, Gamma, integer), type(N, Gamma, integer).
type(mult(M, N), Gamma, integer) :- type(M, Gamma, integer), type(N, Gamma, integer).
type(eq(L, R), Gamma, boolean) :- type(L, Gamma, T), type(R, Gamma, T).
type(lt(L, R), Gamma, boolean) :- type(L, Gamma, T), type(R, Gamma, T).
type(let_(Var, Bound, Body), Gamma, Ttype) :- type(Bound, Gamma, Btype), type(Body, cons(pair(Var, Btype), Gamma), Ttype).
type(if_(Cond, Thn, Els), Gamma, Ttype) :- type(Cond, Gamma, boolean), type(Thn, Gamma, Ttype), type(Els, Gamma, Ttype).
assoco(X, cons(pair(X, V), Tl), V).
assoco(X, cons(pair(A, B), Tl), V) :- assoco(X, Tl, V).