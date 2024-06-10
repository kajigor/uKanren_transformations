multiply y0 y1 = (fresh q1, q2 in (((y1 == Succ (q1) & y0 == Zero & multiply Zero q1) | (y1 == Succ (q1) & y0 == Succ (q2) & addMultiply q1 q2))));

addMultiply y2 y4 = (fresh q1, q2 in (((y4 == Zero & y2 == Succ (Succ (Succ (q1))) & _multiply q1) | (y4 == Succ (q2) & _addMultiply y2 q2))));

_multiply y5 = y5 == Zero;

_addMultiply y6 y8 = (fresh q1, q2 in (((y8 == Zero & y6 == Succ (q1) & __multiply q1) | (y8 == Succ (q2) & __addMultiply y6 q2))));

__multiply y9 = y9 == Zero;

__addMultiply y10 y12 = (y12 == Succ (Zero) & ___multiply y10);

___multiply y13 = y13 == Zero;


? multiply x0 x1