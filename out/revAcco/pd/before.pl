revacco(nil, nil) :- success().
revacco(cons(X2, X3), Y1) :- success(), _revacco(Y1, X3, cons(X2, nil)).
_revacco(Y4, nil, Y4).
_revacco(Y2, cons(X6, X7), Y4) :- __revacco(Y2, X7, cons(X6, Y4)).
__revacco(Y5, Y6, Y7) :- _revacco(Y5, Y6, Y7).