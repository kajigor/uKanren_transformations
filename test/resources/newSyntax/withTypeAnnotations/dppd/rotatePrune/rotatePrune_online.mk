prune y0 =
  (fresh q1, q2, q3 in
    (((y0 == Tree (Tree (Leaf (O)) (S (O)) (Leaf (S (O)))) (S (O)) (q1) & _prune q1) |
    (y0 == Tree (Tree (Tree (q2) (O) (q3)) (S (O)) (Leaf (S (O)))) (S (O)) (q1) & _prune q1))));

_prune y1 = (fresh q1, q2 in ((y1 == Leaf (O) | y1 == Tree (q1) (O) (q2))));


? prune x0