map5(F,I,A,Xs) :- geo(I,s(s(s(s(s(o))))),true).
map5(F,I,A,[H|T]) :- lto(I,s(s(s(s(s(o))))),true), apply(F, A, I, H), map5(F, s(I), A, T).