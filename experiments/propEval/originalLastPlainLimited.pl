evalo(St, conj(X, Y), U, s(N)) :- evalo(St, X, V, N), evalo(St, Y, W, N), ando(V, W, U).
evalo(St, disj(X, Y), U, s(N)) :- evalo(St, X, V, N), evalo(St, Y, W, N), oro(V, W, U).
evalo(St, neg(X), U, s(N)) :- evalo(St, X, V, N), noto(V, U).
evalo(St, var(Z), U, o) :- elemo(Z, St, U).
ando(true, true, true).
ando(false, true, false).
ando(true, false, false).
ando(false, false, false).
oro(true, true, true).
oro(false, true, true).
oro(true, false, true).
oro(false, false, false).
noto(true, false).
noto(false, true).
elemo(o, cons(H, T), H).
elemo(s(N1), cons(H, T), V) :- elemo(N1, T, V).