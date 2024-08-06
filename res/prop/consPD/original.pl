fail() :- fail().
ando(trueo, trueo, trueo).
ando(falso, trueo, falso).
ando(trueo, falso, falso).
ando(falso, falso, falso).
oro(trueo, trueo, trueo).
oro(falso, trueo, trueo).
oro(trueo, falso, trueo).
oro(falso, falso, falso).
noto(trueo, falso).
noto(falso, trueo).
implicationo(falso, trueo, trueo).
implicationo(falso, falso, trueo).
implicationo(trueo, trueo, trueo).
implicationo(trueo, falso, falso).
evalo(St, lit(U), U).
evalo(St, var(Z), U) :- elemo(Z, St, U).
evalo(St, disj(X, Y), U) :- oro(V, W, U), evalo(St, X, V), evalo(St, Y, W).
evalo(St, conj(X, Y), U) :- ando(V, W, U), evalo(St, X, V), evalo(St, Y, W).
elemo(zero, cons(H, T), H).
elemo(succ(N_0), cons(H, T), V) :- elemo(N_0, T, V).