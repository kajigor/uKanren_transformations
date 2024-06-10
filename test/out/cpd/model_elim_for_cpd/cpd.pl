solve(Y0, Y1, Y2) :- contrapositiveProveall(Y0, Y1, Y2).
contrapositiveProveall(Y3, Y4, Y5) :- input_clauseProveall(Y3, Y4, Y5).
input_clauseProveall(nil, Y9, Y9).
input_clauseProveall(cons(Q1, Q3), Y8, cons(Q1, Q2)) :- prove(Q3, Y8, Q2).
prove(Y11, Y12, Y13) :- contrapositiveProveall(Y11, Y12, Y13).