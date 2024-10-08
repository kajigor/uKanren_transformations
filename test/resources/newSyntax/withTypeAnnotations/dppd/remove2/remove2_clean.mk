rr x y =
  fresh t in
    f x t &
    f t y;

f x y =
  x == Nil &
  y == Nil |
  (
    fresh a, t in
      x == (a :: t) &
      h a t y
  );

h a t y =
  t == Nil &
  y == [a] |
  (
    fresh b, s in
      t == (b :: s) &
      g a b t s y
  );

neq a b =
  (fresh t in
    a == Zero &
    b == Succ t |
    a == Succ t &
    b == Zero) |
  (fresh t, t1 in
    a == Succ t &
    b == Succ t1 &
    neq t t1
  );

g a b t s y =
  fresh a1, y1 in
    y == (a1 :: y1) &
    a == a1 &
    (
      a == b &
      f s y1 |
      neq a b &
      f t y1
    );

? rr x [Succ Zero, Zero, Succ (Succ Zero)]