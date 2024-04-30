filter (static static dynamic)
appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    xy == (h :: ty) &
    appendo t y ty &
    x == (h :: t)
  );

filter (dynamic static)
reverso x y =
  x == [] & y == [] |
  (fresh h, t, rt in
    appendo rt [h] y &
    x == (h :: t) &
    reverso t rt
  );

filter (dynamic dynamic dynamic static)
  help xs ys zs ts = reverso zs ts & appendo xs ys ts;

? help a b c [Succ (Succ Zero), Zero, Succ Zero, Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero, Zero, Succ (Succ Zero)]
