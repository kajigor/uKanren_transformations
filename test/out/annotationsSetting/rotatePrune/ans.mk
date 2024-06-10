filter (dynamic static) 
 prune t1 t2 = ((fresh n in ((t2 == Leaf n & t1 == Leaf n))) | (fresh l, n, r, n1 in ((n == 0 & t2 == Leaf n1 & t1 == Tree l n r & n1 == 0))) | (fresh l, nl, r, pl, pr, nr, n in ((t2 == Tree pl nr pr & nr == (1 + n) & nl == (1 + n) & Unfold prune l pl & Unfold prune r pr & t1 == Tree l nl r))));
filter (dynamic static) 
 rotate t1 t2 = ((fresh n in ((t2 == Leaf n & t1 == Leaf n))) | (fresh l, n, r, rl, n, rr in ((t2 == Tree rl n rr & Unfold rotate l rl & Unfold rotate r rr & t1 == Tree l n r))) | (fresh l, n, r, rl, n, rr in ((t2 == Tree rr n rl & Unfold rotate l rl & Unfold rotate r rr & t1 == Tree l n r))));
filter (dynamic static) 
 rp t1 t2 = (fresh u in ((Unfold prune u t2 & Unfold rotate t1 u)));
filter () 
 fail  = Memo fail [];

(fresh x in (Unfold prune x (Tree Tree Leaf 0 1 Leaf 1 1 Leaf 0)))