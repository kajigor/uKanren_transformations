fail() :- fail().
transpose(From, nil) :- nullrows(From).
transpose(From, cons(Y, Ys)) :- transpose(Zs, Ys), makerow(From, Y, Zs).
makerow(nil, nil, nil).
makerow(cons(cons(H, Xs3), Xs2), cons(H, T1), cons(Xs3, T2)) :- makerow(Xs2, T1, T2).
nullrows(nil).
nullrows(cons(nil, T)) :- nullrows(T).