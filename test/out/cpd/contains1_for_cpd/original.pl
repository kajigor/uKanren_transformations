fail() :- fail().
containso(Pat, Str) :- cono(Str, nil, Pat).
cono(Str, Prefix, nil).
cono(cons(H, Rem), Prefix, Postfix) :- newo(H, Prefix, Postfix, New_prefix, New_postfix), cono(Rem, New_prefix, New_postfix).
newo(T, Prefix, cons(T, New_postfix), New_prefix, New_postfix) :- appendo1(Prefix, cons(T, nil), New_prefix).
newo(T, Prefix, cons(H, Rem_postfix), New_prefix, New_postfix) :- appendo1(Prefix, cons(T, nil), Temp), appendo2(New_prefix, Rest, Prefix), appendo2(S0, New_prefix, Temp), appendo1(Rest, cons(H, Rem_postfix), New_postfix).
appendo1(nil, Ys, Ys).
appendo1(cons(H, T), Ys, cons(H, Ts)) :- appendo1(T, Ys, Ts).
appendo2(nil, Ys, Ys).
appendo2(cons(H, T), Ys, cons(H, Ts)) :- appendo2(T, Ys, Ts).