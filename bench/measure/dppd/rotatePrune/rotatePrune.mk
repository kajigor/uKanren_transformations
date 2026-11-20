rp t1 t2 =
  fresh u in
    prune u t2 &
    rotate t1 u;

rotate t1 t2 =
  (
    fresh n in
      t1 == Leaf n & 
      t2 == Leaf n
  ) |
  (
    fresh l, n, r, rl, n, rr in
      t1 == Tree l n r & 
      rotate l rl &
      rotate r rr &
      t2 == Tree rl n rr
  ) |
  (
    fresh l, n, r, rl, n, rr in
      t1 == Tree l n r &
      rotate l rl &
      rotate r rr &
      t2 == Tree rr n rl 
  );

prune t1 t2 =
  (
    fresh n in 
      t1 == Leaf n &
      t2 == Leaf n 
  ) |
  (
    fresh l, r in
      t2 == Leaf O &
      t1 == Tree l O r
  ) |
  (
    fresh l, nl, r, pl, pr, nr, n in 
      t1 == Tree l nl r &
      nl == S n &
      prune l pl &
      prune r pr &
      nr == S n &
      t2 == Tree pl nr pr 
  );

? prune x (Tree (Tree (Leaf O) (S O) (Leaf (S O)) ) (S O) (Leaf O))