sumtrsquaretr(o, Y1, Y2, Y3) :- squareAdd(o, Q1, Q2), squareAdd(Y1, Y3, Q1), square(Y2, Q2).
sumtrsquaretr(s(Q4), Y1, Y2, s(Q3)) :- addMultiplySquareAddAddAdd(Q4, Q5, Q6, Q3), square(Y1, Q5), square(Y2, Q6).
squareAdd(o, Y6, Y6).
squareAdd(s(Q2), s(Q1), Y6) :- addMultiplyAdd(Y6, Q2, Q1).
addMultiplyAdd(Y12, o, Y12).
addMultiplyAdd(Y8, s(Q2), s(Q1)) :- _addMultiply(Q2, Q3, s(Q2), Q2), addAdd(Y8, Q2, s(s(Q3)), Q1).
addAdd(Y13, o, Y15, Y17) :- add(Y13, Y15, Y17).
addAdd(Y13, s(Q2), Y15, s(Q1)) :- addAdd(Y13, Q2, Y15, Q1).
multiply(Y18, o, o).
multiply(Y18, s(Q2), s(Q1)) :- _addMultiply(Y18, Q1, Y18, Q2).
add(Y23, o, Y23).
add(Y21, s(Q2), s(Q1)) :- add(Y21, Q2, Q1).
square(o, o).
square(s(Q2), s(Q1)) :- _addMultiply(Q2, Q1, Q2, Q2).
_addMultiply(o, Y31, Y32, Y33) :- multiply(Y32, Y33, Y31).
_addMultiply(s(Q2), s(Q1), Y32, Y33) :- _addMultiply(Q2, Q1, Y32, Y33).
addMultiplySquareAddAddAdd(o, Y38, Y40, Y42) :- squareAddAdd(Y42, Y38, Y40, o).
addMultiplySquareAddAddAdd(s(Q3), Y38, Y40, s(Q1)) :- squareAddAdd(Q2, Y38, Y40, s(Q3)), _addMultiply(Q3, Q4, s(Q3), Q3), addAdd(Q2, Q3, s(s(Q4)), Q1).
squareAddAdd(Y43, Y45, Y47, o) :- add(s(Y47), Y45, Y43).
squareAddAdd(Y43, Y45, Y47, s(Q1)) :- addMultiplyAddAdd(Y43, Y45, Y47, Q1).
addMultiplyAddAdd(Y49, Y50, Y51, o) :- add(s(s(s(s(Y51)))), Y50, Y49).
addMultiplyAddAdd(Y49, Y50, Y51, s(Q1)) :- addAddAddMultiplyAddAdd(Y49, Y50, Y51, Q1).
addAddAddMultiplyAddAdd(Y56, Y57, Y58, o) :- add(s(s(s(s(s(s(s(s(s(Y58))))))))), Y57, Y56).
addAddAddMultiplyAddAdd(Y56, Y57, Y58, s(Q1)) :- _addMultiply(Q1, Q2, s(s(s(Q1))), Q1), addAdd(Y58, Q1, s(s(s(s(Q3)))), Q4), add(s(s(s(s(Q5)))), Q1, Q3), add(s(s(s(s(Q2)))), Q1, Q5), add(s(s(s(s(Q4)))), Y57, Y56).