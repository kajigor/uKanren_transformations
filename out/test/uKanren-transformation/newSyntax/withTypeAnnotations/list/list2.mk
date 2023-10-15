filter (static dynamic dynamic)
appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

filter (dynamic static)
reverso x y =
  x == [] & y == [] |
  (fresh h, t, rt in
    x == (h :: t) &
    appendo rt [h] y &
    reverso t rt 
  );

? appendo xs ys ts & reverso ts ys
