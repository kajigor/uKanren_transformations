nthOpt(nil, none).
nthOpt(cons(Q1, nil), none).
nthOpt(cons(Q1, cons(Q2, nil)), none).
nthOpt(cons(Q1, cons(Q2, cons(Q3, nil))), none).
nthOpt(cons(Q1, cons(Q2, cons(Q3, cons(Q4, Q5)))), some(Q4)).