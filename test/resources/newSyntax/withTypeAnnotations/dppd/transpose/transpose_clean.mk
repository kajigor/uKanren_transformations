transpose from to =
  to == Nil &
  nullrows from |
  (fresh y, ys, zs in
    to == (y :: ys) &
    transpose zs ys &
    makerow from y zs
  );

makerow x y z =
  x == Nil &
  y == Nil &
  z == Nil |
  (
    fresh xs1, xs2, x1, xs3, t1, t2, h in
      y == (h :: t1) &
      z == (xs3 :: t2) &
      x == (xs1 :: xs2) &
      makerow xs2 t1 t2 &
      xs1 == (h :: xs3)
  );

nullrows xs =
  xs == Nil |
  (
    fresh t in
      nullrows t &
      xs == (Nil :: t)
  );

? transpose x ([[O, S O, S (S O)], [S O, S (S O), O], [S O, S (S O), O]])