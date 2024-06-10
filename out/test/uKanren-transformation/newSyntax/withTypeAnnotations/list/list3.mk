filter (static static dynamic)
appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

filter (static dynamic)
reverso x y =
  x == [] & y == [] |
  (fresh h, t, rt in
    reverso t rt &
    x == (h :: t) &
    appendo rt [h] y
  );

? appendo xs ys ts & reverso ts ys
