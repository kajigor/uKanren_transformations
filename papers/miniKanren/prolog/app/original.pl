appendo(nil, Y, Y).
appendo(cons(H, T), Y, cons(H, Ty)) :- appendo(T, Y, Ty).

?- appendo(X,Y,cons(1,cons(2,cons(3,cons(4,cons(5,cons(6,cons(7,cons(8,cons(9,cons(10,cons(11,cons(12,cons(13,cons(14,cons(15,nil)))))))))))))))).
