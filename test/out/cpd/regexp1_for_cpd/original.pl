fail() :- fail().
generate(empty, Lst, Lst).
generate(char(X), cons(X, Lst), Lst).
generate(or(X, Y), Str, Lst) :- generate(X, Str, Lst).
generate(or(X, Y), Str, Lst) :- generate(Y, Str, Lst).
generate(cat(X, Y), Str, Lst) :- generate(X, Str, T1), generate(Y, T1, Lst).
generate(star(X), Lst, Lst).
generate(star(X), Str, Lst) :- generate(X, Str, T1), generate(star(X), T1, Lst).