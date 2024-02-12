generate(Y0, Y1, Y2, Y3, nil).
generate(Y0, Y1, Y2, Y3, cons(Y0, cons(Y2, Q1))) :- generate(Y0, Y1, Y2, Y3, Q1).
generate(Y0, Y1, Y2, Y3, cons(Y0, cons(Y3, Q1))) :- generate(Y0, Y1, Y2, Y3, Q1).
generate(Y0, Y1, Y2, Y3, cons(Y1, cons(Y2, Q1))) :- generate(Y0, Y1, Y2, Y3, Q1).
generate(Y0, Y1, Y2, Y3, cons(Y1, cons(Y3, Q1))) :- generate(Y0, Y1, Y2, Y3, Q1).