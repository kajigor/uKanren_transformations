fail() :- fail().
containso(Pat, Str) :- cono(Str, nil, Pat).
cono(Str, Prefix, nil).
cono(cons(H, Rem), Prefix, Postfix) :- newo(H, Prefix, Postfix, New_prefix, New_postfix), cono(Rem, New_prefix, New_postfix).
newo(T, Prefix, cons(T, New_postfix), New_prefix, New_postfix) :- appendo(Prefix, cons(T, nil), New_prefix).
newo(T, Prefix, cons(H, Rem_postfix), New_prefix, New_postfix) :- appendo(Prefix, cons(T, nil), Temp), appendo(New_prefix, Rest, Prefix), appendo(S0, New_prefix, Temp), appendo(Rest, cons(H, Rem_postfix), New_postfix).
appendo(nil, Ys, Ys).
appendo(cons(H, T), Ys, cons(H, Ts)) :- appendo(T, Ys, Ts).