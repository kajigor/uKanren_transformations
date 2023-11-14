filter (dynamic)
 nullrows xs =
  (xs == [] |
  (fresh h, t in
    ((Memo nullrows t & xs == (h :: t) & h == Nil))));

filter (dynamic static dynamic)
 makerow x y z =
  ((x == [] & y == [] & z == []) |
   (fresh xs1, xs2, x1, xs3, t1, t2, h in
    ((y == (h :: t1) & z == (xs3 :: t2) & x == (xs1 :: xs2) & Unfold makerow xs2 t1 t2 & xs1 == (h :: xs3)))));

filter (dynamic static)
 transpose from to =
  ((to == [] & Unfold nullrows from) |
   (fresh y, ys, zs in
    ((to == (y :: ys) & Unfold transpose zs ys & Unfold makerow from y zs))));

filter ()
 fail  = Memo fail [];

(fresh x in (Unfold transpose x ([[O, S O, S (S O)], [S O, S (S O), O], [S O, S (S O), O]])))
