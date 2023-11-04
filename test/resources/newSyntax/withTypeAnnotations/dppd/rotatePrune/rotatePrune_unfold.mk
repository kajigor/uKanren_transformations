prune y0 =
  (fresh q1, q2, q3, q4 in
    ((y0 == Tree (Tree (Leaf (O)) (S (O)) (Leaf (S (O)))) (S (O)) (Leaf (O)) |
      y0 == Tree (Tree (Leaf (O)) (S (O)) (Leaf (S (O)))) (S (O)) (Tree (q1) (O) (q2)) |
      y0 == Tree (Tree (Tree (q3) (O) (q4)) (S (O)) (Leaf (S (O)))) (S (O)) (Leaf (O)) |
      y0 == Tree (Tree (Tree (q3) (O) (q4)) (S (O)) (Leaf (S (O)))) (S (O)) (Tree (q1) (O) (q2)))));


? prune x0