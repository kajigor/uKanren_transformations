neq x y =
  (fresh t in
    x == O &
    y == S t
  ) |
  (fresh t in
    x == S t &
    y == O
  ) |
  (fresh tx, ty in
    x == S tx &
    y == S ty &
    neq tx ty
  );

rr x y =
  fresh t in
    r x t &
    r t y;

r x y =
  (
    x == Nil &
    y == Nil
  ) |
  ( fresh a, t, t1 in
    x == (a :: a :: t) &
    y == (a :: t1) &
    r t t1
  ) |
  (
    fresh ax, ay, t, t1 in
    x == (ax :: ay :: t) &
    y == (ax :: t1) &
    neq ax ay &
    r (ay :: t) t1
  );

? rr ([S O, S O, S O, S O]) y