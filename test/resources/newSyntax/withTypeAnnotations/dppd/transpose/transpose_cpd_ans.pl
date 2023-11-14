fail() :- fail().
transpose(cons(cons(o, cons(s(o), cons(s(o), nil))), cons(cons(s(o), cons(s(s(o)), cons(s(s(o)), Q1))), cons(cons(s(s(o)), cons(o, cons(o, Q2))), nil)))) :- nullrows(Q1, Q2).
nullrows(nil, nil).