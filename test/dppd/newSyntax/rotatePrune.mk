rp t1 t2 =
  fresh u in rotate t1 u & prune u t2;

rotate t1 t2 =
  (fresh n in
     t1 == Leaf n & t2 == t1) | 
  (fresh l, n, r, rl, rr in
     t1 == Tree l n r & t2 == Tree rl n rr & rotate l rl & rotate r rr) | 
  (fresh l, n, r, rl, rr in
     t1 == Tree l n r & t2 == Tree rr n rl & rotate l rl & rotate r rr);

addo x y z =
  x == O & z == y | (fresh x1, z1 in x == S x1 & z == S z1 & addo x1 y z1);

prune t1 t2 =
  (fresh n in
     t1 == Leaf n & t2 == t1) | 
  (fresh l, r, z in
     z == O & t1 == Tree l z r & t2 == Leaf z) | 
  (fresh l, n, r, pl, pr in
     n1 == S n & t1 == Tree l n1 r & t2 == Tree pr n1 pl & rotate l pl & rotate r pr);

? fresh t1, t2 in
    rp t1 t2