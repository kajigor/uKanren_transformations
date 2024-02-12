match(nil).
match(cons(s(s(o)), Q1)) :- match1(Q1).
match1(nil).
match1(cons(s(o), Q1)) :- _match1(Q1).
_match1(nil).
_match1(cons(s(o), Q1)) :- __match1(Q1).
__match1(nil).
__match1(cons(o, Q1)) :- ___match1(Q1).
___match1(nil).
___match1(cons(o, Q1)) :- ____match1(Q1).
____match1(nil).
____match1(cons(s(s(o)), Q1)) :- _____match1(Q1).
_____match1(nil).
_____match1(cons(o, Q1)) :- ______match1(Q1).
______match1(nil).
______match1(cons(s(o), nil)).