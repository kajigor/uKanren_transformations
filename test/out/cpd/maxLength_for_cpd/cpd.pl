max_length(Y0, succ(succ(succ(succ(succ(zero)))))) :- max1(Y0).
max1(Y2) :- _max1(Y2).
_max1(Y3) :- __max1(Y3).
__max1(Y4) :- ___max1(Y4).
___max1(succ(succ(zero))).