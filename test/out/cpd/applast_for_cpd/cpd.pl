applasto(nil, Y1, Y2) :- lasto(Y1, Y2).
applasto(cons(Q1, Q2), Y1, Y2) :- appendoLasto(Y1, Y2, Q1, Q2).
lasto(Y4, Y4).
appendoLasto(Y5, Y6, Y7, nil) :- lasto(Y5, Y6).
appendoLasto(Y5, Y6, Y7, cons(Q1, Q2)) :- appendoLasto(Y5, Y6, Q1, Q2).