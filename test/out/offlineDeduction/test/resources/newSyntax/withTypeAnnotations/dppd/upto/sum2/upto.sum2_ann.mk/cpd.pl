sumtrsquaretr(o, Y1, Y2, Y3) :- multiplyAdd(Y2, Y3, Q1), multiply(Y1, Y1, Q1).
sumtrsquaretr(s(Q3), Y1, Y2, s(Q2)) :- addMultiplyAddMultiplyAddAddAdd(Q3, Q4, Q5, Q2), multiply(Y2, Y2, Q4), multiply(Y1, Y1, Q5).
multiplyAdd(o, Y7, Y7).
multiplyAdd(s(Q2), s(Q1), Y7) :- _addAdd(Y7, Q2, Q3, Q1), multiply(s(Q2), Q2, Q3).
multiply(Y8, o, o).
multiply(Y8, s(Q1), Y10) :- _add(Q2, Y8, Y10), multiply(Y8, Q1, Q2).
_add(Y16, o, Y16).
_add(Y14, s(Q2), s(Q1)) :- _add(Y14, Q2, Q1).
addMultiplyAddMultiplyAddAddAdd(o, Y21, Y22, Y26) :- _add(s(Y22), Y21, Y26).
addMultiplyAddMultiplyAddAddAdd(s(Q5), Y21, Y22, s(Q1)) :- __addAdd(s(s(Q2)), Q3, Q4, Q5, Q2), __addAdd(s(s(Q6)), Q7, Q8, Q5, Q6), multiply(s(s(Q5)), Q5, Q4), multiply(s(s(Q5)), Q5, Q8), _add(Y22, Q7, Q9), _add(s(s(Q9)), Y21, Q10), _add(Q10, Q3, Q1).
_addAdd(Y32, o, Y34, Y36) :- _add(Y32, Y34, Y36).
_addAdd(Y32, s(Q2), Y34, s(Q1)) :- _addAdd(Y32, Q2, Y34, Q1).
__addAdd(Y38, Y38, Y39, o, Y41) :- _add(Y39, o, Y41).
__addAdd(Y37, s(Q3), Y39, s(Q2), s(Q1)) :- __addAdd(Y37, Q3, Y39, Q2, Q1).