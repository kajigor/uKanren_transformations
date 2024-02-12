multiply y0 y1 = (fresh q1, q2, q3 in (((y1 == Succ (q1) & y0 == Zero & multiply Zero q1) | (y1 == Succ (Succ (Succ (Succ (q2)))) & y0 == Succ (Zero) & _multiply q2) | (y1 == Succ (Succ (q3)) & y0 == Succ (Succ (Zero)) & __multiply q3) | (y1 == Succ (q1) & y0 == Succ (Succ (Succ (Succ (Zero)))) & ___multiply q1))));

_multiply y2 = y2 == Zero;

__multiply y3 = y3 == Zero;

___multiply y4 = y4 == Zero;


? multiply x0 x1