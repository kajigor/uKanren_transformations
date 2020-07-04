type_(bConst_(X), Gamma, some(boolean)).
type_(iConst_(Y), Gamma, some(integer)).
type_(var_(V), Gamma, Ttype) :- idx(V, Gamma, Ttype).
type_(plus_(M, N), Gamma, some(integer)) :- type_(M, Gamma, some(integer)), type_(N, Gamma, some(integer)).
type_(mult_(M, N), Gamma, some(integer)) :- type_(M, Gamma, some(integer)), type_(N, Gamma, some(integer)).
type_(equal_(L, R), Gamma, some(boolean)) :- type_(L, Gamma, some(T1)), type_(R, Gamma, some(T1)).
type_(less_(L, R), Gamma, some(boolean)) :- type_(L, Gamma, some(integer)), type_(R, Gamma, some(integer)).
type_(let_(Bound, Body), Gamma, Ttype) :- type_(Bound, Gamma, some(Btype1)), type_(Body, cons(Btype1, Gamma), Ttype).
type_(if_(Cond, Thn, Els), Gamma, Ttype) :- type_(Cond, Gamma, some(boolean)), type_(Thn, Gamma, Ttype), type_(Els, Gamma, Ttype).
idx(o, nil, none).
idx(o, cons(H, T), some(H)).
idx(s(K1), cons(H, T), V) :- idx(K1, T, V).