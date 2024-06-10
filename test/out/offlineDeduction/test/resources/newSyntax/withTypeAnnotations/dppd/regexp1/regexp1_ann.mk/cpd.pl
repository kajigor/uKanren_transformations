generate(Y0, Y1, Y2, Y3, nil).
generate(Y0, Y1, Y2, Y3, Y4) :- generateGenerate(Y0, Y1, Y2, Y3, Y4, Q1), generate(Y0, Y1, Y2, Y3, Q1).
generateGenerate(Y5, Y6, Y7, Y8, cons(Y5, Q1), Y10) :- _generate(Y7, Y8, Y10, Q1).
generateGenerate(Y5, Y6, Y7, Y8, cons(Y6, Q1), Y10) :- _generate(Y7, Y8, Y10, Q1).
_generate(Y12, Y13, Y14, cons(Y12, Y14)).
_generate(Y12, Y13, Y14, cons(Y13, Y14)).