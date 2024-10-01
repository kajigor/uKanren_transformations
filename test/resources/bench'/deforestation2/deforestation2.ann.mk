filter (dynamic static)
 r x y =
  ((x == [] & y == []) |
  (fresh a, t, t1 in
    ((x == (a :: a :: t) &
      y == (a :: t1) &
      Unfold r t t1))) |
      (fresh ax, ay, t, t1 in
        ((x == (ax :: ay :: t) &
          y == (ax :: t1) &
          Unfold neq ax ay &
          Unfold r ((ay :: t)) t1))));

filter (dynamic static)
 rr x y =
  (fresh t in
    ((Unfold r t y &
    Unfold r x t)));

filter (dynamic static)
 neq x y =
  ((fresh t in
    ((x == O & y == S t))) |
     (fresh t in
      ((x == S t & y == O))) |
     (fresh tx, ty in
      ((x == S tx &
        y == S ty &
        Unfold neq tx ty))));

filter ()
 fail  = Memo fail [];

(fresh x, y in (Unfold rr x [S O, O, S (S O), O, O]))