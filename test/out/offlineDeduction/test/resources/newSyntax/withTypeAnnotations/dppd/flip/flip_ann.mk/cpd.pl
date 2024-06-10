flipflip(leaf(Q1), leaf(Q1)).
flipflip(tree(Q6, Q3, Q5), tree(Q2, Q3, Q4)) :- _flipFlip(Q5, Q4), _flipFlip(Q6, Q2).
_flipFlip(leaf(Q1), leaf(Q1)).
_flipFlip(tree(Q6, Q3, Q5), tree(Q2, Q3, Q4)) :- _flipFlip(Q5, Q4), _flipFlip(Q6, Q2).