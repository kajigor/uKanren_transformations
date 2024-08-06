revacco(nil, Acc, Acc).
revacco(cons(H, T), Acc, Sx) :- revacco(T, cons(H, Acc), Sx).