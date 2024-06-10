applasto(nil).
applasto(cons(Q1, Q2)) :- appendoLasto(Q1, Q2).
appendoLasto(Y1, nil).
appendoLasto(Y1, cons(Q1, Q2)) :- appendoLasto(Q1, Q2).