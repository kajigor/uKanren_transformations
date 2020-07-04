evalo(St, conj(X, Y), U) :- ando(V, W, U), evalo(St, X, V), evalo(St, Y, W).
evalo(St, disj(X, Y), U) :- oro(V, W, U), evalo(St, X, V), evalo(St, Y, W).
evalo(St, neg(X), U) :- noto(V, U), evalo(St, X, V).
evalo(St, var(Z), U) :- elemo(Z, St, U).
ando(A, B, C) :- nando(A, B, Ab), nando(Ab, Ab, C).
nando(false, false, true).
nando(false, true, true).
nando(true, false, true).
nando(true, true, false).
oro(A, B, C) :- nando(A, A, Aa), nando(B, B, Bb), nando(Aa, Bb, C).
noto(A, Na) :- nando(A, A, Na).
elemo(o, cons(H, T), H).
elemo(s(N1), cons(H, T), V) :- elemo(N1, T, V).