matcho p s =
  fresh t1, t2, s1 in
    appendo s1 t1 s &
    appendo t2 p s1;

appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

? matcho x ([S (S O), S O, S O, O, O, S (S O), O, S O])