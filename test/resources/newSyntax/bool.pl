fail() :- fail().
nando(falso, falso, trueo).
nando(falso, trueo, trueo).
nando(trueo, falso, trueo).
nando(trueo, trueo, falso).
noto(A, Na) :- nando(A, A, Na).
oro(A, B, C) :- nando(A, A, Aa), nando(B, B, Bb), nando(Aa, Bb, C).
ando(A, B, C) :- nando(A, B, Ab), nando(Ab, Ab, C).