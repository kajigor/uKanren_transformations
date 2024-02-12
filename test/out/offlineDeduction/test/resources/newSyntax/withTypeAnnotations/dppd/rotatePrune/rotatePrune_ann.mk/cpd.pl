prune(tree(tree(leaf(o), s(o), leaf(s(o))), s(o), Q1)) :- _prune(Q1).
prune(tree(tree(tree(Q2, o, Q3), s(o), leaf(s(o))), s(o), Q1)) :- _prune(Q1).
_prune(leaf(o)).
_prune(tree(Q1, o, Q2)).