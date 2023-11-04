rp t1 t2 =
  fresh u in
    prune u t2 &
    rotate t1 u;

rotate t1 t2 =
  (
    fresh n in
      t2 == Leaf n &
      t1 == Leaf n
  ) |
  (
    fresh l, n, r, rl, n, rr in
      t2 == Tree rl n rr &
      rotate l rl &
      rotate r rr &
      t1 == Tree l n r
  ) |
  (
    fresh l, n, r, rl, n, rr in
      t2 == Tree rr n rl &
      rotate l rl &
      rotate r rr &
      t1 == Tree l n r
  );

prune t1 t2 =
  (
    fresh n in
      t2 == Leaf n &
      t1 == Leaf n
  ) |
  (
    fresh l, n, r, n1 in
      n == O &
      t2 == Leaf n1 &
      t1 == Tree l n r &
      n1 == O
  ) |
  (
    fresh l, nl, r, pl, pr, nr, n in
      t2 == Tree pl nr pr &
      nr == S n &
      nl == S n &
      prune l pl &
      prune r pr &
      t1 == Tree l nl r
  );

? prune x (Tree (Tree (Leaf O) (S O) (Leaf (S O)) ) (S O) (Leaf O))