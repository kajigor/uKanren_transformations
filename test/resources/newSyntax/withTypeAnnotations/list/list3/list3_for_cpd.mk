appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    xy == (h :: ty) &
    appendo t y ty &
    x == (h :: t)
  );

reverso x y =
  x == [] & y == [] |
  (fresh h, t, rt in
    x == (h :: t) &
    appendo rt [h] y &
    reverso t rt
  );

help xs ys zs ts = appendo xs ys ts & reverso zs ts;

? help a b c [Succ (Succ Zero), Zero, Succ Zero, Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero, Zero, Succ (Succ Zero)]
