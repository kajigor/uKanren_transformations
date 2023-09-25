appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

reverso x y =
  x == [] & y == [] |
  (fresh h, t, rt in
    x == (h :: t) &
    reverso t rt &
    appendo rt [h] y
  );


help xs ys ts =
  appendo xs ys ts & reverso ts ys;

? help xs ys ts
