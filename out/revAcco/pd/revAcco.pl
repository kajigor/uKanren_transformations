revacco(nil, nil).
revacco(cons(Q1, Q2), Y1) :- _revacco(Y1, Q2, cons(Q1, nil)).
_revacco(Y4, nil, Y4).
_revacco(Y2, cons(Q1, Q2), Y4) :- __revacco(Y2, Q2, cons(Q1, Y4)).
__revacco(Y5, Y6, Y7) :- _revacco(Y5, Y6, Y7).