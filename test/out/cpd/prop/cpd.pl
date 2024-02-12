evalo(Y0) :- ___evaloEvalo(Y0, trueo).
evalo(Y0) :- __evaloEvalo(Y0, trueo).
evalo(Y0) :- ___evaloEvalo(Y0, falso).
andoEvaloEvaloEvaloEvalo(Y5, Y6, Y7, Y8) :- __evaloEvalo(Y5, Y7), ___evaloEvalo(Y6, Y8).
andoEvaloEvaloEvaloEvalo(Y5, Y6, Y7, Y8) :- ___evaloEvalo(Y5, Y7), __evaloEvalo(Y6, Y8).
andoEvaloEvaloEvaloEvalo(Y5, Y6, Y7, Y8) :- __evaloEvalo(Y5, Y7), __evaloEvalo(Y6, Y8).
__evaloEvalo(lit(falso), falso).
__evaloEvalo(neg(Q1), Y10) :- ___evaloEvalo(Q1, Q2), noto(Y10, Q2).
__evaloEvalo(disj(Q1, Q3), Y10) :- __evaloEvalo(Q1, Q2), __evaloEvalo(Q3, Q4), oro(Y10, Q2, Q4).
__evaloEvalo(conj(Q1, Q3), Y10) :- andoEvaloEvaloEvaloEvalo(Q1, Q3, Q2, Q4), ando(Y10, Q2, Q4).
__evaloEvalo(impl(Q1, Q3), Y10) :- ___evaloEvalo(Q1, Q2), __evaloEvalo(Q3, Q4), implicationo(Y10, Q2, Q4).
noto(falso, trueo).
noto(trueo, falso).
oro(trueo, trueo, trueo).
oro(trueo, falso, trueo).
oro(trueo, trueo, falso).
oro(falso, falso, falso).
ando(trueo, trueo, trueo).
ando(falso, falso, trueo).
ando(falso, trueo, falso).
ando(falso, falso, falso).
implicationo(trueo, falso, trueo).
implicationo(trueo, falso, falso).
implicationo(trueo, trueo, trueo).
implicationo(falso, trueo, falso).
___evaloEvalo(lit(trueo), trueo).
___evaloEvalo(neg(Q1), Y23) :- __evaloEvalo(Q1, Q2), noto(Y23, Q2).
___evaloEvalo(disj(Q1, Q3), Y23) :- oroEvaloEvaloEvaloEvalo(Q1, Q3, Q2, Q4), oro(Y23, Q2, Q4).
___evaloEvalo(conj(Q1, Q3), Y23) :- ___evaloEvalo(Q1, Q2), ___evaloEvalo(Q3, Q4), ando(Y23, Q2, Q4).
___evaloEvalo(impl(Q1, Q3), Y23) :- implicationoEvaloEvaloEvaloEvalo(Q1, Q3, Q2, Q4), implicationo(Y23, Q2, Q4).
oroEvaloEvaloEvaloEvalo(Y26, Y27, Y28, Y29) :- ___evaloEvalo(Y26, Y28), ___evaloEvalo(Y27, Y29).
oroEvaloEvaloEvaloEvalo(Y26, Y27, Y28, Y29) :- __evaloEvalo(Y26, Y28), ___evaloEvalo(Y27, Y29).
oroEvaloEvaloEvaloEvalo(Y26, Y27, Y28, Y29) :- ___evaloEvalo(Y26, Y28), __evaloEvalo(Y27, Y29).
implicationoEvaloEvaloEvaloEvalo(Y32, Y33, Y34, Y35) :- __evaloEvalo(Y32, Y34), ___evaloEvalo(Y33, Y35).
implicationoEvaloEvaloEvaloEvalo(Y32, Y33, Y34, Y35) :- __evaloEvalo(Y32, Y34), __evaloEvalo(Y33, Y35).
implicationoEvaloEvaloEvaloEvalo(Y32, Y33, Y34, Y35) :- ___evaloEvalo(Y32, Y34), ___evaloEvalo(Y33, Y35).