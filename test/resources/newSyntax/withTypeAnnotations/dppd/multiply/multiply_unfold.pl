fail() :- fail().
multiply(zero, succ(Q1)) :- multiply(zero, Q1).
multiply(succ(Q2), succ(Q1)) :- addMultiply(Q1, Q2).
addMultiply(succ(succ(succ(Q1))), zero) :- _multiply(Q1).
addMultiply(Y2, succ(Q2)) :- _addMultiply(Y2, Q2).
_multiply(zero).
_addMultiply(succ(Q1), zero) :- __multiply(Q1).
_addMultiply(Y6, succ(Q2)) :- __addMultiply(Y6, Q2).
__multiply(zero).
__addMultiply(Y10, succ(zero)) :- ___multiply(Y10).
___multiply(zero).